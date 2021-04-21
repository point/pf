(ns pf.core
  (:require-macros [hiccups.core :as hiccups :refer [html]])
  (:require [cljs.core.async :refer [go go-loop]]
            [cljs.core.async :as async :refer [<! >! close! put!]]
            [hiccups.runtime :as hiccupsrt :refer [h]]
            [goog.string :as gs]) 
  (:import [goog.date Date UtcDateTime]
           [goog.i18n DateTimeFormat NumberFormat]))

(def state (atom {}))

(defn subscribe [path]
  (let [id (gensym "watch") 
        c (async/chan 512)]
    (add-watch 
      state
      id 
      (fn [key ref old new] 
        (when-not (= (get-in old path) (get-in new path)) 
          (when-some [val (get-in new path)]
            (put! c val)))))
    c
    ))

(defn- format-date-iso [d]
  (str (.getUTCFullYear d)
       "-" (gs/padNumber (inc (.getUTCMonth d)) 2)
       "-" (gs/padNumber (.getUTCDate d) 2)
       "T" (gs/padNumber (.getUTCHours d) 2)
       ":" (gs/padNumber (.getUTCMinutes d) 2)
       ":" (gs/padNumber (.getUTCSeconds d) 2)
       "." (gs/padNumber (.getUTCMilliseconds d) 3)
       "Z"))

(defn- format-date-str [d]
  (->
    DateTimeFormat/Format.LONG_DATE
    (DateTimeFormat. )
    (.format  d)))

(defn- format-currency [v]
  (->
    NumberFormat/Format.CURRENCY
    (NumberFormat. )
    (.format v)
    (.substring 1)))

(defn update-state 
  ([path value] (update-state path value false))
  ([path value force?]
   (cond
     (map? value) (case force?
                    true (swap! state assoc-in path value)
                    false (swap! state update-in path merge value))
     (fn? value)  (swap! state update-in path value)
     (nil? value) (swap! state update-in (vec (butlast path)) dissoc (last path))
     :else (swap! state assoc-in path value))))

(defn get-state 
  ([path] (get-state path nil))
  ([path not-found]
   (get-in @state path not-found)))

(defn q  
  ([selector] (.querySelector js/document selector))
  ([el selector] (when el (.querySelector el selector))))

(defn element-from-html [html]
  (let [template (.createElement js/document "template")]
    (set! (.-innerHTML template) html)
    (-> template .-content .-firstChild)))


(defn expenses-row-template [datetime amount description]
  [:tr 
   [:td [:time {:datetime (format-date-iso datetime)} (format-date-str datetime)]]
   [:td (str "₴" (format-currency amount))]
   [:td description]])

(defn- by-month-chart-data [rows]
  (let [data-by-months (group-by #(.format (DateTimeFormat. "MMMM") (:datetime %1)) rows)
        labels (keys data-by-months)
        data-for-chart (clj->js
                         (map 
                           (fn [[_month records]]
                             (->>
                               records
                               (map :amount)
                               (reduce +))) 
                           data-by-months))]
    [labels data-for-chart]))
  
(defn- by-month-chart [canvas rows backgrounds]
  (let [ctx (.getContext canvas "2d")
        [labels data] (by-month-chart-data rows)
        params {:type "bar",
                :data {:labels labels
                       :datasets [{:label "Expenses"
                                   :data data
                                   :backgroundColor backgrounds}]}}]
    (js/Chart. ctx (clj->js params))))

(defn- by-day-chart-data [rows]
  (let [last-month (->> rows (map #(.getMonth (:datetime %1))) sort last)
        rows-for-a-month (filter #(= last-month (.getMonth (:datetime %1))) rows)
        labels (clj->js (map :description rows-for-a-month))
        data-for-chart (clj->js (map :amount rows-for-a-month))]
    [labels data-for-chart]))

(defn- by-day-chart [canvas rows backgrounds]
  (let [ctx (.getContext canvas "2d")
        [labels data] (by-day-chart-data rows)
        params {:type "pie",
                :data {:labels labels
                       :datasets [{:label "Expenses"
                                   :data data
                                   :backgroundColor backgrounds}]}
               :options { :plugins { :legend { :position "right"}}} }]
    (js/Chart. ctx (clj->js params))))

(let [c (subscribe [:expenses])]
  (go-loop 
    []
    (when-some [_new-state (<! c)]
      (when-let [tbody (q "table tbody")]
        (while (.-firstChild tbody)
          (.removeChild tbody (.-firstChild tbody)))
        (doseq [row (sort #(> (:datetime %1) (:datetime %2)) (get-state [:expenses] []))]
          (.appendChild 
            tbody 
            (-> 
              (expenses-row-template (:datetime row) (:amount row) (:description row))
              html
              element-from-html
              ))))

      (when-let [by-month-chart (get-state [:by-month-chart])]
        (let [[_labels data] (by-month-chart-data (get-state [:expenses]))]
          (when (some-> by-month-chart .-data .-datasets (nth 0))
            (-> by-month-chart .-data .-datasets (nth 0) .-data (set! data))
            (.update by-month-chart))))

      (when-let [by-day-chart (get-state [:by-day-chart])]
        (let [[labels data] (by-day-chart-data (get-state [:expenses]))]
          (when (some-> by-day-chart .-data .-datasets (nth 0))
            (-> by-day-chart .-data .-datasets (nth 0) .-data (set! data))
            (-> by-day-chart .-data .-labels (set! labels))
            (.update by-day-chart))))

      (recur))))

(.addEventListener 
  js/document 
  "DOMContentLoaded" 
  (fn [] 
    (when-let [by-month (q "#by_month")]
      (when-let [by-day (q "#by_day")]
        (let [backgrounds ["rgba(255, 99, 132, 0.2)", "rgba(54, 162, 235, 0.2)", "rgba(255, 206, 86, 0.2)",
                          "rgba(75, 192, 192, 0.2)", "rgba(153, 102, 255, 0.2)", "rgba(255, 159, 64, 0.2)"]
              rows (get-state [:expenses] [])
              chart1 (by-month-chart by-month rows backgrounds)
              chart2 (by-day-chart by-day rows backgrounds)
              ]
          (update-state [:by-month-chart] chart1)
          (update-state [:by-day-chart] chart2)
          )))))

(.addEventListener 
  js/document 
  "DOMContentLoaded" 
  (fn [] 
    (when-let [form (q "form")]
      (.addEventListener
        form
        "submit"
        (fn [ev]
          (.preventDefault ev)
          (let [date (-> "#date" q .-value UtcDateTime/fromIsoString)
                amount (-> "#amount" q .-value js/parseFloat)
                description (-> "#description" q .-value)]
            (update-state [:expenses] #(conj %1 {:datetime date :description description :amount amount}))
            (.reset form)))))))


(update-state 
  [:expenses] 
  [{:datetime (UtcDateTime/fromIsoString "2021-01-10") :description "Methow Conservancy" :amount 250.00}
   {:datetime (UtcDateTime/fromIsoString "2021-01-11") :description "Zeitgeist Coffee, Seattle, WA" :amount 10.1}
   {:datetime (UtcDateTime/fromIsoString "2021-01-12") :description "Shell Oil, Auto Fuel Dispenser" :amount 38.69}
   {:datetime (UtcDateTime/fromIsoString "2021-01-13") :description "Winthrop Mountain Sports Winthrop, WA" :amount 39.32}
   {:datetime (UtcDateTime/fromIsoString "2021-01-14") :description "Toyota of Seattle, Seattle, WA" :amount 251.38}
   {:datetime (UtcDateTime/fromIsoString "2021-01-15") :description "Amazon.com" :amount 25.81}
   {:datetime (UtcDateTime/fromIsoString "2021-01-16") :description "Chase Bank Mortgage" :amount 1903.00}
   {:datetime (UtcDateTime/fromIsoString "2021-01-17") :description "Audible" :amount 9.12}
   {:datetime (UtcDateTime/fromIsoString "2021-01-18") :description "Storyville Coffee, Seattle, WA" :amount 8.93}
   {:datetime (UtcDateTime/fromIsoString "2021-01-19") :description "Hank's Harvest Foodstwisp WA" :amount 46.46}
   {:datetime (UtcDateTime/fromIsoString "2021-01-20") :description "Ikea Seattle, Renton, WA" :amount 246.54}
   {:datetime (UtcDateTime/fromIsoString "2021-01-21") :description "Itunes.com" :amount 1.59}
   {:datetime (UtcDateTime/fromIsoString "2021-01-22") :description "Mazama Store, Mazama" :amount 15.53}
   {:datetime (UtcDateTime/fromIsoString "2021-01-23") :description "New York Times Digital" :amount 18.08}
   {:datetime (UtcDateTime/fromIsoString "2021-01-24") :description "Roadpost Usa" :amount 35.85}
   {:datetime (UtcDateTime/fromIsoString "2021-01-25") :description "Chevron Service Stn" :amount 27.69}
   {:datetime (UtcDateTime/fromIsoString "2021-01-26") :description "Texaco Service Stn" :amount 25.28}
   {:datetime (UtcDateTime/fromIsoString "2021-01-27") :description "Costco Gas, Burlington" :amount 19.16}
   {:datetime (UtcDateTime/fromIsoString "2021-01-28") :description "Dropbox" :amount 6.19}
   {:datetime (UtcDateTime/fromIsoString "2021-01-29") :description "Zoka Coffee Roa, Seattle, WA" :amount 2.1}
   {:datetime (UtcDateTime/fromIsoString "2021-01-30") :description "Okanogan County Energy, Winthrop, WA" :amount 164.35}
   {:datetime (UtcDateTime/fromIsoString "2021-01-31") :description "Alaska Air" :amount 156.1}
   {:datetime (UtcDateTime/fromIsoString "2021-02-01") :description "Seattle YMCA" :amount 400}
   {:datetime (UtcDateTime/fromIsoString "2021-02-02") :description "Trader Joe's #130, Seattle" :amount 69.1}
   {:datetime (UtcDateTime/fromIsoString "2021-02-03") :description "Roadpost Usa" :amount 56.98}
   {:datetime (UtcDateTime/fromIsoString "2021-02-04") :description "Chevron Service Stn" :amount 37.62}
   {:datetime (UtcDateTime/fromIsoString "2021-02-05") :description "Tiller (tillerhq.com)" :amount 10}
   {:datetime (UtcDateTime/fromIsoString "2021-02-06") :description "Shell Oil, Auto Fuel Dispenser" :amount 47.83}
   {:datetime (UtcDateTime/fromIsoString "2021-02-07") :description "Winthrop Mountain Sports Winthrop, WA" :amount 55.82}
   {:datetime (UtcDateTime/fromIsoString "2021-02-08") :description "Alaska Wilderness League" :amount 400}
   {:datetime (UtcDateTime/fromIsoString "2021-02-09") :description "Big Star Montessori" :amount 740}
   {:datetime (UtcDateTime/fromIsoString "2021-02-10") :description "Itunes.com" :amount 2.99}
   {:datetime (UtcDateTime/fromIsoString "2021-02-11") :description "Mazama Store, Mazama" :amount 18.18}
   {:datetime (UtcDateTime/fromIsoString "2021-02-12") :description "Toyota of Seattle, Seattle, WA" :amount 435.11}
   {:datetime (UtcDateTime/fromIsoString "2021-02-13") :description "Amazon.com" :amount 30}
   {:datetime (UtcDateTime/fromIsoString "2021-02-14") :description "PCC Natural Market - Issaquah WA" :amount 23}
   {:datetime (UtcDateTime/fromIsoString "2021-02-15") :description "Espresso Vivace Seattle, Seattle, WA" :amount 7.25}
   {:datetime (UtcDateTime/fromIsoString "2021-02-16") :description "The Essential Baking Co, Seattle, WA" :amount 16.35}
   {:datetime (UtcDateTime/fromIsoString "2021-02-17") :description "AT&T" :amount 125.43}
   {:datetime (UtcDateTime/fromIsoString "2021-02-18") :description "Zeitgeist Coffee, Seattle, WA" :amount 12.51}
   {:datetime (UtcDateTime/fromIsoString "2021-02-19") :description "Centurylink" :amount 43.73}
   {:datetime (UtcDateTime/fromIsoString "2021-02-20") :description "Portage Bay Cafe and Cate, Seattle, WA" :amount 72.28}
   {:datetime (UtcDateTime/fromIsoString "2021-02-21") :description "Whole Foods - Seattle WA" :amount 108.91}
   {:datetime (UtcDateTime/fromIsoString "2021-02-22") :description "Chase Bank Mortgage" :amount 1903.00}
   {:datetime (UtcDateTime/fromIsoString "2021-02-23") :description "Audible" :amount 16.18}
   {:datetime (UtcDateTime/fromIsoString "2021-02-24") :description "REI, Store 11, Seattle" :amount 513.97}
   {:datetime (UtcDateTime/fromIsoString "2021-02-25") :description "Union Garage" :amount 50}
   {:datetime (UtcDateTime/fromIsoString "2021-02-26") :description "Evergreen Iga Market" :amount 36.76}
   {:datetime (UtcDateTime/fromIsoString "2021-02-27") :description "Glover Street Market" :amount 35.34}
   {:datetime (UtcDateTime/fromIsoString "2021-02-28") :description "Costco Gas, Burlington" :amount 19.67}
   {:datetime (UtcDateTime/fromIsoString "2021-03-01") :description "Texaco Service Stn" :amount 48.74}
   {:datetime (UtcDateTime/fromIsoString "2021-03-02") :description "New York Times Digital" :amount 20}
   {:datetime (UtcDateTime/fromIsoString "2021-03-03") :description "Dropbox" :amount 10.81}
   {:datetime (UtcDateTime/fromIsoString "2021-03-04") :description "Hank's Harvest Foodstwisp WA" :amount 52.69}
   {:datetime (UtcDateTime/fromIsoString "2021-03-05") :description "Storyville Coffee, Seattle, WA" :amount 12.91}
   {:datetime (UtcDateTime/fromIsoString "2021-03-06") :description "Home Depot" :amount 251.33}
   {:datetime (UtcDateTime/fromIsoString "2021-03-07") :description "Zoka Coffee Roa, Seattle, WA" :amount 3.56}
   {:datetime (UtcDateTime/fromIsoString "2021-03-08") :description "Okanogan County Energy, Winthrop, WA" :amount 322.84}
   {:datetime (UtcDateTime/fromIsoString "2021-03-09") :description "Alaska Air" :amount 115.75}
   {:datetime (UtcDateTime/fromIsoString "2021-03-10") :description "Evergreen Iga Market" :amount 28.6}
   {:datetime (UtcDateTime/fromIsoString "2021-03-11") :description "Glover Street Market" :amount 27.97}
   {:datetime (UtcDateTime/fromIsoString "2021-03-12") :description "Big Star Montessori" :amount 621.17}
   {:datetime (UtcDateTime/fromIsoString "2021-03-13") :description "United Way" :amount 300}
   {:datetime (UtcDateTime/fromIsoString "2021-03-14") :description "Tiller (tillerhq.com)" :amount 6.87}
   {:datetime (UtcDateTime/fromIsoString "2021-03-15") :description "PCC Natural Market - Issaquah WA" :amount 14.63}
   {:datetime (UtcDateTime/fromIsoString "2021-03-16") :description "Espresso Vivace Seattle, Seattle, WA" :amount 6.18}
   {:datetime (UtcDateTime/fromIsoString "2021-03-17") :description "The Essential Baking Co, Seattle, WA" :amount 15.96}
   {:datetime (UtcDateTime/fromIsoString "2021-03-18") :description "Shell Oil, Auto Fuel Dispenser" :amount 25.17}
   {:datetime (UtcDateTime/fromIsoString "2021-03-19") :description "Winthrop Mountain Sports Winthrop, WA" :amount 50.35}
   {:datetime (UtcDateTime/fromIsoString "2021-03-20") :description "REI, Store 11, Seattle" :amount 488.96}
   {:datetime (UtcDateTime/fromIsoString "2021-03-21") :description "Toyota of Seattle, Seattle, WA" :amount 260.37}
   {:datetime (UtcDateTime/fromIsoString "2021-03-22") :description "Amazon.com" :amount 16.49}
   {:datetime (UtcDateTime/fromIsoString "2021-03-23") :description "Hank's Harvest Foodstwisp WA" :amount 27.18}
   {:datetime (UtcDateTime/fromIsoString "2021-03-24") :description "Khan Academy" :amount 300}
   {:datetime (UtcDateTime/fromIsoString "2021-03-25") :description "Roadpost Usa" :amount 34.22}
   {:datetime (UtcDateTime/fromIsoString "2021-03-26") :description "Chevron Service Stn" :amount 25.75}
   {:datetime (UtcDateTime/fromIsoString "2021-03-27") :description "Texaco Service Stn" :amount 44.81}
   {:datetime (UtcDateTime/fromIsoString "2021-03-28") :description "AT&T" :amount 102.85}
   {:datetime (UtcDateTime/fromIsoString "2021-03-29") :description "Costco Gas, Burlington" :amount 13.77}
   {:datetime (UtcDateTime/fromIsoString "2021-03-30") :description "Zeitgeist Coffee, Seattle, WA" :amount 10.26}
   {:datetime (UtcDateTime/fromIsoString "2021-03-31") :description "Dropbox" :amount 9.71}
   {:datetime (UtcDateTime/fromIsoString "2021-04-01") :description "Storyville Coffee, Seattle, WA" :amount 11.73}
   {:datetime (UtcDateTime/fromIsoString "2021-04-02") :description "Chase Bank Mortgage" :amount 1903.00}
   {:datetime (UtcDateTime/fromIsoString "2021-04-03") :description "Audible" :amount 15.12}
   {:datetime (UtcDateTime/fromIsoString "2021-04-04") :description "North Valley Lumber" :amount 249.68}
   {:datetime (UtcDateTime/fromIsoString "2021-04-05") :description "Itunes.com" :amount 2.08}
   {:datetime (UtcDateTime/fromIsoString "2021-04-06") :description "Mazama Store, Mazama" :amount 16.07}
   {:datetime (UtcDateTime/fromIsoString "2021-04-07") :description "Trader Joe's #130, Seattle" :amount 68.52}
   {:datetime (UtcDateTime/fromIsoString "2021-04-08") :description "New York Times Digital" :amount 18.12}
   {:datetime (UtcDateTime/fromIsoString "2021-04-09") :description "Centurylink" :amount 24.42}
   {:datetime (UtcDateTime/fromIsoString "2021-04-10") :description "Whole Foods - Seattle WA" :amount 107.76}
   {:datetime (UtcDateTime/fromIsoString "2021-04-11") :description "Portage Bay Cafe and Cate, Seattle, WA" :amount 60.21}
   {:datetime (UtcDateTime/fromIsoString "2021-04-12") :description "Union Garage" :amount 25.41}
   {:datetime (UtcDateTime/fromIsoString "2021-04-13") :description "Zoka Coffee Roa, Seattle, WA" :amount 2.52}
   {:datetime (UtcDateTime/fromIsoString "2021-04-14") :description "Okanogan County Energy, Winthrop, WA" :amount 205.44}])

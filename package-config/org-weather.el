;; Load the org-weather library
(require 'org-weather)
;; Set your location and refresh the data
(setq org-weather-location "Paris,FR"
      org-weather-format "Weather: %desc, %tmin-%tmax%tu, %p%pu, %h%hu, %s%su")
(org-weather-refresh)

;; Load the org-weather library
(require 'org-weather)
;; Set your location and refresh the data
(setq org-weather-location "Jouy-en-josas,FR"
      org-weather-format "Weather: %desc, %tmin-%tmax%tu, %h%hu, %s%su"
      org-weather-api-key "8b0a4c33ea005be33324f6aeb8120991")
(org-weather-refresh)

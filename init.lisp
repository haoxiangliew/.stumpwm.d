;; haoxiangliew's StumpWM configuration

(require :clx-truetype)
(require :xembed)

(in-package :clx-truetype)

(defparameter +font-cache-filename+
  #.(merge-pathnames "font-cache.sexp"
                     (merge-pathnames ".fonts/" (user-homedir-pathname))))

(setq *font-dirs*
      (append (list
	       (namestring "/usr/share/fonts")
	       (namestring "/run/current-system/sw/share/X11/fonts"))
	      *font-dirs*))

(in-package :stumpwm)

;; ttf-fonts
(add-to-load-path (concat "~/.stumpwm.d/stumpwm-contrib/util/ttf-fonts"))
(load-module "ttf-fonts")

(if (not (find "JetBrains Mono" (xft:get-font-families)
               :test #'equal))
    (xft:cache-fonts))

(set-font
 (make-instance 'xft:font
                 :family "JetBrains Mono"
                 :subfamily "Regular"
		 :size 10))
		 ;; :antialias t))

;; HACK
;; define a function to clear caches for clx-truetype
;; run at startup and on a timer to prevent memory leaks
(defcommand clx-clean() ()
	    (lambda ()
	      (loop for font in (screen-fonts (current-screen))
		    when (typep font 'xft:font)
		    do (clrhash (xft::font-string-line-bboxes font))
		    (clrhash (xft::font-string-line-alpha-maps font))
		    (clrhash (xft::font-string-bboxes font))
		    (clrhash (xft::font-string-alpha-maps font)))))

(run-with-timer 900 900 (clx-clean))

;; (set-font "-misc-jetbrains mono-medium-r-normal-*-13-*-*-*-m-0-iso10646-1")

;; async-run
(defparameter *async-shell* (uiop:launch-program "bash" :input :stream :output :stream))
(defun async-run (command)
  (write-line command (uiop:process-info-input *async-shell*))
  (force-output (uiop:process-info-input *async-shell*))
  (let* ((output-string (read-line (uiop:process-info-output *async-shell*)))
         (stream (uiop:process-info-output *async-shell*)))
    (if (listen stream)
        (loop while (listen stream)
              do (setf output-string (concatenate 'string
                                                  output-string
                                                  '(#\Newline)
                                                  (read-line stream)))))
    output-string))

;; stumptray
(add-to-load-path (concat "~/.stumpwm.d/stumpwm-contrib/modeline/stumptray"))
(load-module "stumptray")

;; end-session
(add-to-load-path (concat "~/.stumpwm.d/stumpwm-contrib/util/end-session"))
(load-module "end-session")

;; amixer
(add-to-load-path (concat "~/.stumpwm.d/stumpwm-contrib/media/amixer"))
(load-module "amixer")

(amixer::defvolcontrol amixer-Master-5- "Master" "5%-")
(amixer::defvolcontrol amixer-Master-5+ "Master" "5%+")

(define-key *top-map* (kbd "XF86AudioLowerVolume") "amixer-Master-5-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "amixer-Master-5+")
(define-key *top-map* (kbd "XF86AudioMute") "amixer-Master-toggle")

;; backlight
(defcommand backlight-increase() ()
	    (run-shell-command "light -A 5")
	    (message
	     (concat "Backlight: " (run-shell-command "light -G" t))))
(defcommand backlight-decrease() ()
	    (run-shell-command "light -U 5")
	    (message
	     (concat "Backlight: " (run-shell-command "light -G" t))))

(define-key *top-map* (kbd "XF86MonBrightnessUp") "backlight-increase")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "backlight-decrease")

;; battery-portable %B
(add-to-load-path (concat "~/.stumpwm.d/stumpwm-contrib/modeline/battery-portable"))
(load-module "battery-portable")

(setf battery-portable::*no-battery-info* "(unknown)")

;; (defun get-battery()
;;   (async-run "acpi | head -n 1 | sed 's/Battery 0: //g'"))

;; cpu %c
(add-to-load-path (concat "~/.stumpwm.d/stumpwm-contrib/modeline/cpu"))
(load-module "cpu")

(setf cpu::*cpu-modeline-fmt* "%c")

;; mem %M
(add-to-load-path (concat "~/.stumpwm.d/stumpwm-contrib/modeline/mem"))
(load-module "mem")

(setf mem::*mem-modeline-fmt* "MEM: %p")

;; net %l
(add-to-load-path (concat "~/.stumpwm.d/stumpwm-contrib/modeline/net"))
(load-module "net")

;; swm-gaps
(add-to-load-path (concat "~/.stumpwm.d/stumpwm-contrib/util/swm-gaps"))
(load-module "swm-gaps")

(setf swm-gaps:*inner-gaps-size* 5)

(swm-gaps:toggle-gaps-on)

;; which-key mode
(which-key-mode)

;; startup
(defcommand refresh-contrib() ()
	    (run-shell-command "kitty -e '/home/haoxiangliew/.stumpwm.d/update-contrib.sh'"))
(defcommand system-refresh() ()
	    (run-shell-command "autorandr --change")
	    (run-shell-command "feh --bg-scale ~/haoxiangliew/Wallpapers/eboy-dracula-tokyo.png")
	    (run-shell-command "xsetwacom --set 'HID 256c:006d Pad pad' Button 1 'key +ctrl +z -z -ctrl'")
	    (run-shell-command "xsetwacom --set 'HID 256c:006d Pad pad' Button 2 'key +ctrl +y -y -ctrl'")
	    (run-shell-command "xsetwacom --set 'HID 256c:006d Pad pad' Button 3 'key +ctrl +1 -1 -ctrl'")
	    (run-shell-command "xsetwacom --set 'HID 256c:006d Pad pad' Button 8 'key +ctrl +6 -6 -ctrl'")
	    (run-shell-command "xsetwacom --set 'HID 256c:006d Pad pad' Button 9 'key +ctrl +n -n -ctrl'")
	    (run-shell-command "xsetwacom --set 'HID 256c:006d Pad pad' Button 10 'key +ctrl +o -o -ctrl'")
	    (run-shell-command "xsetwacom --set 'HID 256c:006d Pad pad' Button 11 'key +ctrl +s -s -ctrl'")
	    (run-shell-command "xsetwacom --set 'HID 256c:006d Pad pad' Button 12 'key +ctrl +e -e -ctrl'")
	    (run-shell-command "xinput map-to-output 'HID 256c:006d Pen stylus' DisplayPort-2")
	    (run-shell-command "xinput map-to-output 'HID 256c:006d Pad pad' DisplayPort-2"))
(defcommand system-startup() ()
	    ;; (run-shell-command "bash ~/.stumpwm.d/update-contrib.sh")
	    (run-shell-command "caffeine")
	    (run-shell-command "caprine")
	    (run-shell-command "discord --start-minimized")
	    (run-shell-command "feh --bg-scale ~/haoxiangliew/Wallpapers/eboy-dracula-tokyo.png")
	    (run-shell-command "kitty emacs --daemon")
	    (run-shell-command "ibus-daemon")
	    (run-shell-command "nm-applet")
	    (run-shell-command "numlockx")
	    (run-shell-command "picom -b --experimental-backends")
	    (run-shell-command "blueman-applet")
	    (run-shell-command "pulseeffects --gapplication-service")
	    (run-shell-command "solaar -w hide")
	    (run-shell-command "udiskie -t")
	    (run-shell-command "xsetroot -cursor_name left_ptr"))

(system-refresh)
(system-startup)
(clx-clean)

;; set prefix key to Super + t
(set-prefix-key (kbd "s-t"))

;; automatic window numbers
(add-hook *destroy-window-hook* #'(lambda (win) (repack-window-numbers)))

;; focus on mouse click
(setf *mouse-focus-policy* :click)

;; theming
(let ((bg "#1e2029")
      (fg "#f8f8f2")
      (hl "#6272a4"))

  (set-fg-color fg)
  (set-bg-color bg)
  (set-border-color bg)
  (set-focus-color hl)
  (set-unfocus-color bg)

  (setf *mode-line-foreground-color* fg
        *mode-line-border-width* 1
        *mode-line-background-color* bg
        *mode-line-highlight-template* "^R~A^r")

  (setf *grab-pointer-character* 40
        *grab-pointer-character-mask* 41))

(set-msg-border-width 1)

(setf *input-window-gravity* :center
      *message-window-gravity* :center
      *message-window-padding* 15
      *message-window-y-padding* 20)

(setf *colors*
      '("#1e2029" ;; 0 black
	"#ff5555" ;; 1 red
	"#50fa7b" ;; 2 green
	"#f1fa8c" ;; 3 yellow
	"#0189cc" ;; 4 blue
	"#ff79c6" ;; 5 magenta
	"#8be9fd" ;; 6 cyan
	"#f8f8f2" ;; 7 white
	))

(update-color-map (current-screen))

;; commands and keybinds

(defun swap-groups (group1 group2)
  (rotatef (slot-value group1 'number) (slot-value group2 'number)))
(defun move-group-forward (&optional (group (current-group)))
  (swap-groups group (next-group group (sort-groups (current-screen)))))
(defun move-group-backward (&optional (group (current-group)))
  (swap-groups group (next-group group (reverse (sort-groups (current-screen))))))

(defcommand gbackward() ()
	    (move-group-backward)
	    (echo-groups (current-screen) *group-format*))
(defcommand gforward() ()
	    (move-group-forward)
	    (echo-groups (current-screen) *group-format*))

(define-key *groups-map* (kbd "Left") "gbackward")
(define-key *groups-map* (kbd "Right") "gforward")

(defvar *my-map* (make-sparse-keymap))

(define-key *root-map* (kbd "O") '*my-map*)

(defcommand chrome() ()
	    (run-shell-command "google-chrome-stable --enable-features=VaapiVideoDecoder,WebUIDarkMode --use-gl=desktop --disable-features=UseOzonePlatform --force-dark-mode"))
(defcommand chrome-incognito() ()
	    (run-shell-command (concat "google-chrome-stable --enable-features=VaapiVideoDecoder,WebUIDarkMode --use-gl=desktop --disable-features=UseOzonePlatform --force-dark-mode" " -incognito")))
(defcommand lock() ()
	    (run-shell-command "xautolock -locknow"))

(define-key *my-map* (kbd "b") "chrome")
(define-key *my-map* (kbd "n") "chrome-incognito")

(defun rofi (mode)
  (run-shell-command (concat "rofi -show " mode)))

(defcommand rofi-run() ()
  (rofi "run"))
(defcommand rofi-drun() ()
  (rofi "drun"))
(defcommand rofi-window() ()
  (rofi "window"))

(define-key *root-map* (kbd "!") "rofi-run")
(define-key *root-map* (kbd "@") "rofi-drun")
(define-key *root-map* (kbd "#") "rofi-window")

(define-key *root-map* (kbd "ESC") "system-refresh")

(defvar *maim-map* (make-sparse-keymap))

(define-key *my-map* (kbd "m") '*maim-map*)

(defun maim (mode)
  (run-shell-command (concat "maim " mode)))

(defcommand maim-clip() ()
	    (maim "-s -m 10 -u | xclip -selection clipboard -t image/png -i"))
(defcommand maim-shot() ()
	    (maim "-s -m 10 ~/Pictures/$(date +%s).png"))

(define-key *maim-map* (kbd "c") "maim-clip")
(define-key *maim-map* (kbd "s") "maim-shot")

(defvar *playerctl-map* (make-sparse-keymap))

(define-key *my-map* (kbd "p") '*playerctl-map*)

(defun playerctl (mode)
  (run-shell-command (concat "playerctl " mode)))

(defcommand playerctl-play-pause() ()
	    (playerctl "play-pause"))
(defcommand playerctl-status() ()
	    (run-shell-command "notify-send -t 5000 \"$(playerctl metadata --format \"{{ status }}: {{ artist }} - {{ title }}\")\""))
(defcommand playerctl-next() ()
	    (playerctl "next"))
(defcommand playerctl-previous() ()
	    (playerctl "previous"))

(define-key *playerctl-map* (kbd "Up") "playerctl-play-pause")
(define-key *playerctl-map* (kbd "Down") "playerctl-status")
(define-key *playerctl-map* (kbd "Left") "playerctl-previous")
(define-key *playerctl-map* (kbd "Right") "playerctl-next")

(undefine-key *root-map* (kbd "c"))
(undefine-key *root-map* (kbd "C-c"))

(defcommand terminal() ()
	    (run-shell-command "kitty"))

(define-key *root-map* (kbd "c") "terminal")
(define-key *root-map* (kbd "C-c") "terminal")

(undefine-key *root-map* (kbd "e"))
(undefine-key *root-map* (kbd "C-e"))

(defvar *emacsclient-map* (make-sparse-keymap))

(defcommand emacs() ()
	    (run-shell-command "emacs"))
(defcommand start-daemon() ()
	    (run-shell-command "kitty emacs --daemon"))
(defcommand emacsclient() ()
	    (run-shell-command "emacsclient -c"))
(defcommand eshell() ()
	    (run-shell-command "emacsclient -c -e '(+eshell/here)'"))
(defcommand vterm() ()
	    (run-shell-command "emacsclient -c -e '(+vterm/here default-directory)'"))
(defcommand dired() ()
	    (run-shell-command "emacsclient -c -e '(dired default-directory)'"))

(define-key *root-map* (kbd "e") "emacs")
(define-key *root-map* (kbd "C-e") '*emacsclient-map*)

(define-key *emacsclient-map* (kbd "s") "start-daemon")
(define-key *emacsclient-map* (kbd "e") "emacsclient")
(define-key *emacsclient-map* (kbd "t") "eshell")
(define-key *emacsclient-map* (kbd "v") "vterm")
(define-key *emacsclient-map* (kbd "d") "dired")

;; modeline
(setf *window-format* " %n %s%m%c ")
(setf *group-format* " %n %s %t ")
(setf *screen-mode-line-format* (list " "
				      "%h "
				      " "
				      "^B^5%g^b "
				      "^B^6%W^b "
				      "^> "
				      " ^7| "
				      "^B^6%C^b "
				      "^7| "
				      "^B^6%M^b "
				      "^7| "
				      "^B^6%l^b "
				      "^7| "
				      "^B%B^b "
				      ;; "^B"
				      ;; '(:eval (get-battery))
				      ;; "^b "
				      "^7| "
				      "^B^7%d^b "
				      "^7| "
				      "                "))

(setf *time-modeline-string* "%a %b %e  %k:%M:%S")

(setf *mode-line-timeout* 1)

(defcommand toggle-all-mode-lines () ()
    (loop for head in (screen-heads (current-screen)) do
	  (toggle-mode-line (current-screen) head))
    (stumptray::stumptray))

(toggle-all-mode-lines)

;; (stumptray::stumptray)

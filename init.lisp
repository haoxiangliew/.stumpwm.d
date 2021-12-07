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
 (list
  (make-instance 'xft:font
                 :family "JetBrains Mono"
                 :subfamily "Regular"
		 :size 10
		 :antialias t)))

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

;; cpu %c
(add-to-load-path (concat "~/.stumpwm.d/stumpwm-contrib/modeline/cpu"))
(load-module "cpu")

;; mem %M
(add-to-load-path (concat "~/.stumpwm.d/stumpwm-contrib/modeline/mem"))
(load-module "mem")

;; net %l
(add-to-load-path (concat "~/.stumpwm.d/stumpwm-contrib/modeline/net"))
(load-module "net")

;; swm-gaps
(add-to-load-path (concat "~/.stumpwm.d/stumpwm-contrib/util/swm-gaps"))
(load-module "swm-gaps")

(setf swm-gaps:*inner-gaps-size* 5)

;; (swm-gaps:toggle-gaps-on)

;; which-key mode
(which-key-mode)

;; startup
(run-shell-command "autorandr --change")
(run-shell-command "bash ~/.stumpwm.d/update-contrib.sh")
(run-shell-command "caffeine")
(run-shell-command "discord --start-minimized")
(run-shell-command "emacs --daemon")
(run-shell-command "ibus-daemon")
(run-shell-command "nm-applet")
(run-shell-command "numlockx")
(run-shell-command "picom -b --experimental-backends")
(run-shell-command "pulseeffects --gapplication-service")
(run-shell-command "solaar -w hide")
(run-shell-command "xsetroot -cursor_name left_ptr")

;; fix scrolling in some programs
(setf (getenv "GDK_CORE_DEVICE_EVENTS") "1")

;; set prefix key to Super + t
(set-prefix-key (kbd "s-t"))

;; window numbers automatically
(add-hook *destroy-window-hook* #'(lambda (win) (repack-window-numbers)))

;; change window focus on mouse click
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
  (setf border-width 1)

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

(setf *menu-maximum-height* 15)

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

;; modeline
(setf *window-format* " %n %s%m%c ")
(setf *group-format* " %n %s %t ")
(setf *screen-mode-line-format* (list " "
				      "%h  "
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
				      "^7| "
				      "^B^7%d^b "
				      "^7| "
				      "        "))

(setf *time-modeline-string* "%a %b %e  %k:%M:%S")

(setf *mode-line-timeout* 1)

(defcommand toggle-all-mode-lines () ()
    (loop for head in (screen-heads (current-screen)) do
      (toggle-mode-line (current-screen) head)))

(toggle-all-mode-lines)

(stumptray::stumptray)

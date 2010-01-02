
;; source files
(set @nu_files		(filelist "^nu/.*nu$"))
(set @m_files		(filelist "^objc/.*.m$"))
(set @nib_files		(filelist "^resources/English\.lproj/.*\.nib$"))

(set @cflags 	"-g -DDARWIN -Iobjc")
(set @ldflags  "-framework Foundation -framework AppKit -framework Nu")

(ifDarwin
         (then
              (set @mflags "-fobjc-exceptions -fobjc-gc"))
         (else (set @cflags "-Wall -g -std=gnu99 -fPIC")
               (set @mflags ((NSString stringWithShellCommand:"gnustep-config --objc-flags") chomp))))

;; framework description
(set @framework					"Nutils")
(set @framework_identifier		"nu.programming.nutils")
(set @framework_creator_code	"????")

(compilation-tasks)
(framework-tasks)

(task "default" => "framework")

(task "clobber" => "clean" is
      (SH "rm -rf #{@framework_dir}"))

(task "default" => "framework")

; (task "doc" is (SH "nudoc"))

(task "install" => "framework" is
      (SH "sudo rm -rf /Library/Frameworks/#{@framework}.framework")
      (SH "ditto #{@framework}.framework /Library/Frameworks/#{@framework}.framework"))

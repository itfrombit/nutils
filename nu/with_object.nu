
(macro-1 with-object (o *body)
     `(progn
            ,@(*body map:
              (do (line)
                  `(,o ,@line)))))

; Example usage:
;
;(import "cocoa")
;
;(set myTextView (((NSTextView alloc) initWithFrame: NSZeroRect)))
;
;(puts (macrox
;(with-object myTextView
;     (setDrawsBackground:NO)
;     (setEditable:NO)
;     (setSelectable:YES)
;     (setTextColor:(NSColor redColor))
;     (setFont:(NSFont controlContentFontOfSize:8))
;     (setTextContainerInset:'(4 4))
;     (setVerticallyResizable:NO))
;))


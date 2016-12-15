#lang racket

(provide screen
         screen-commands
         screen-renderer
         screen-default-buffer
         )

(struct screen (commands renderer default-buffer))
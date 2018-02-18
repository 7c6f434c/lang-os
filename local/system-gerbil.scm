(begin (print "Hello from Gerbil. Time is ") (print (time->seconds (current-time))) (print "\n") (thread-sleep! 300))

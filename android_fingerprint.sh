awk '{print $1}' < ~/.android/adbkey.pub | openssl base64 -d | openssl md5 -c
awk '{print $1}' < ~/.android/adbkey.pub | base64 -d | md5sum



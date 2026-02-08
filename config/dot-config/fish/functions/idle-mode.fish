function idle-mode -d "Lock session and put displays to sleep"
    loginctl lock-session
    sleep 1
    xset dpms force off
end

import time

def countdown(minutes, seconds):
    total_seconds = minutes * 60 + seconds
    while total_seconds:
        mins, secs = divmod(total_seconds, 60)
        timer = '{:02d}:{:02d}'.format(mins, secs)
        with open("countdown_timer.txt", "w") as file:
            file.write(timer)
        time.sleep(1)
        total_seconds -= 1
    print("The Fox is Ready")
    with open("countdown_timer.txt", "w") as file:
        file.write("The Fox is Ready")

# Set the countdown time
minutes = 5
seconds = 45

# Start the countdown
input("Press a button to start")
countdown(minutes, seconds)

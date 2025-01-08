import sys
import os
import subprocess
import random
from calendar import monthrange
from datetime import datetime

script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)


def read_number():
    with open('number.txt', 'r') as f:
        return int(f.read().strip())


def write_number(num):
    with open('number.txt', 'w') as f:
        f.write(str(num))


def git_commit(date):
    # Stage the changes
    subprocess.run(['git', 'add', 'number.txt'])

    # Create commit with current date
    commit_message = f"Continuous stream of small updates on: {date}"
    subprocess.run(['git', 'commit', '--date='+date, '-m', commit_message])

def git_push():
    # Push the committed changes to GitHub
    result = subprocess.run(['git', 'push'], capture_output=True, text=True)
    if result.returncode == 0:
        print("Changes pushed to GitHub successfully.")
    else:
        print("Error pushing to GitHub:")
        print(result.stderr)


def main():
    try:
        date = ""
        year = int(sys.argv[1])
        month = int(sys.argv[2])
        date_range = monthrange(year, month)
        end_date = date_range[1]
        for i in range(end_date):
            start_hour = random.randint(9,14)
            num_commits = random.randint(0,4)
            for _ in range(num_commits):
                date = f'{year}-{month:02}-{(i+1):02} '
                start_minute = random.randint(1,58)
                start_second = random.randint(2,56)
                date += f'{start_hour:02}:{start_minute:02}:{start_second:02}'
                current_number = read_number()
                new_number = current_number + 1
                write_number(new_number)
                git_commit(date)
                git_push()
                start_hour += 1

    except Exception as e:
        print(f"Error: {str(e)}")
        exit(1)


if __name__ == "__main__":
    main() 
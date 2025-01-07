import pandas as pd

# Given the indices of the chosen teams within the csv file, returns a
# dictionary that lists the new team indice and team name, based on the order
# and placement of the indices given in team_numbers
def getTeamsDictionary(team_numbers): 
    data=pd.read_csv("./stadiums.csv")
    team_dictionary = {}
    teams = data['Team']
    for team in range(1,len(team_numbers)+1):
        team_dictionary[team] = teams[team_numbers[team-1]-1]

    return team_dictionary

# Calculates and rounds the distance between two points, (x1,y1) and (x2,y2)
def distance(x1,x2,y1,y2):
    return round(((abs(x1-x2)**2)+(abs(y1-y2)**2))**(1/2))

# Returns a matrix of distances between each team, given the indices of the teams in the csv file
def get_A(team_numbers):
    data=pd.read_csv("./stadiums.csv")
    latitudes=data['Lat']
    longitudes=data['Long']

    A = []
    for team1 in range(len(team_numbers)):
        A.append([])
        for team2 in range(len(team_numbers)):
            A[team1].append(distance(latitudes[team_numbers[team1]],latitudes[team_numbers[team2]],
                                     longitudes[team_numbers[team1]],longitudes[team_numbers[team2]]))
    
    return A
        

import gurobipy as gp
from gurobipy import GRB
from format_A import get_A
from format_A import getTeamsDictionary

T = 4 # number of teams
D = 9 # number of days

# indices in the csv file, ordered by how you want the teams to be put into divisions 1 and 2 
# 1 and 3 in division 1 and 2 and 6 in division 2
team_numbers = [1,3,2,6] 

team_dictionary = getTeamsDictionary(team_numbers) # dictionary used to get team numbers and names
A = get_A(team_numbers) # matrix describing distances between selected teams

# divisons based on the indicies of the teams within the new subset
division_1 = [1,2] 
division_2 = [3,4]

model = gp.Model("model")
model.Params.OutputFlag = 0 # silence gurobi output

x = model.addVars((T**2)*D*2, lb=0, ub=1, vtype=GRB.INTEGER, name='x') # x[(2*T**2)*(l-1)+(2*T)*(i-1)+(2)*(j-1)+(k)] = x on day l, team i, plays team j, at home (k=0) or away (k=1)
y = model.addVars((T**3)*(D), lb=0, ub=1, vtype=GRB.INTEGER, name='y') # y[(T**3)*(l-1)+(T**2)*(i-1)+(T)*(j-1)+(m-1)] = y on day l, team i, moves from arena j, to arena m
z = model.addVars(((T**2)*(D+1)), lb=0, ub=1, vtype=GRB.INTEGER, name='z') # z[(T**2)*(l)+(T)*(i-1)+(j-1)] = z on day l, team i, is in the city of team j
b = model.addVars((T)*(D-1), lb=0, ub=1, vtype=GRB.INTEGER, name='b') # b[(T)*(l-2)+(i-1)] = b on day l, team i does or does not play a back to back game
c = model.addVars(2,lb=0, ub=GRB.INFINITY, vtype=GRB.INTEGER, name='c')# c[0] = maximum number of back to back games a team plays, c[1] = min number of back to back games a team plays

#set objective for minimizing the max difference between teams total back to back games.
for i in range(1,T+1):
    temp_constraint = 0
    for l in range(2,D+1):
        temp_constraint += b[(T)*(l-2)+(i-1)]
    model.addConstr(temp_constraint<=c[0]) # max sum
    model.addConstr(c[1]<=temp_constraint) # min sum
objective1 = c[0] - c[1]

#set objective for minimizing distance
objective2 = A[0][0]*y[0]
for i in range(1,T+1):
    for l in range(0,D):
        for j in range(1,T+1):
            for m in range(1,T+1):
                if not (l == 0 and i == 0 and j == 1 and m == 1):
                    objective2 += A[j-1][m-1]*y[(T**3)*(l)+(T**2)*(i-1)+(T)*(j-1)+(m-1)]

# Teams can not play any games on the fth game of the season
f = 5
for i in range(1,T+1):
    temp_constraint = 0
    for j in range(1,T+1):
        for k in range(0,2):
            temp_constraint += x[(2*T**2)*(f-1)+(2*T)*(i-1)+(2)*(j-1)+(k)]
    model.addConstr(temp_constraint==0)

#Need to track the number of back to backs played by each team
for l in range(2,D+1):
    for i in range(1,T+1):
        temp_constraint2 = 0
        temp_constraint3 = 0
        for j in range(1,T+1):
            for m in range(1,T+1):
                temp_constraint1 = 0
                for k in range(0,2):
                    temp_constraint1 += (x[(2*T**2)*(l-2)+(2*T)*(i-1)+(2)*(j-1)+(k)]+
                                        x[(2*T**2)*(l-1)+(2*T)*(i-1)+(2)*(m-1)+(k)])
                model.addConstr(temp_constraint1-1<=b[(T)*(l-2)+(i-1)])
            
            for k in range(0,2):
                temp_constraint2 += x[(2*T**2)*(l-2)+(2*T)*(i-1)+(2)*(j-1)+(k)]
                temp_constraint3 += x[(2*T**2)*(l-1)+(2*T)*(i-1)+(2)*(j-1)+(k)]
        model.addConstr(b[(T)*(l-2)+(i-1)]<=temp_constraint2)
        model.addConstr(b[(T)*(l-2)+(i-1)]<=temp_constraint3)

#Each team must play each team in the other division d times
d = 1
for i in division_1:
    for j in division_2:
        temp_constraint = x[(2*T)*(i-1)+(2)*(j-1)]
        for l in range(1,D+1):
            for k in range(0,2):
                if not (l == 1 and k == 0):
                    temp_constraint += x[(2*T**2)*(l-1)+(2*T)*(i-1)+(2)*(j-1)+(k)]
        model.addConstr(temp_constraint==d)

#Each team must play each team in their same division e times
e = 2
for d in [division_1,division_2]:
    for i in d:
        for j in d:
            if i != j:
                temp_constraint = x[(2*T)*(i-1)+(2)*(j-1)]
                for l in range(1,D+1):
                    for k in range(0,2):
                        if not (l == 1 and k == 0):
                            temp_constraint += x[(2*T**2)*(l-1)+(2*T)*(i-1)+(2)*(j-1)+(k)]
                model.addConstr(temp_constraint==e)

#Each team must play the same number of home games and away games

for k in range(0,2):
    constraints = []
    for i in range(1,T+1):
        temp_constraint = 0
        for j in range(1,T+1):
            for l in range(1,D+1):
                temp_constraint += x[(2*T**2)*(l-1)+(2*T)*(i-1)+(2)*(j-1)+(k)]
        constraints.append(temp_constraint)
    
    for i in range(1,T+1):
        for j in range(1,T+1):
            if i != j:
                model.addConstr(constraints[i-1]==constraints[j-1])

# Cant play back to back to back against any team
for i in range(1,T+1):
    for l in range(1,D-1):
        temp_constraint = 0
        for j in range(1,T+1):
                for k in range(0,2):
                    temp_constraint += x[(2*T**2)*(l-1)+(2*T)*(i-1)+(2)*(j-1)+(k)] + x[(2*T**2)*(l)+(2*T)*(i-1)+(2)*(j-1)+(k)] + x[(2*T**2)*(l+1)+(2*T)*(i-1)+(2)*(j-1)+(k)]
        model.addConstr(temp_constraint<=2)

# Each team starts at home
for i in range(1,T+1):
    for j in range(1,T+1):
        if i == j:
            model.addConstr(z[T*(i-1)+(j-1)]==1, "temp")

# Teams can't play against themselves
for l in range(1,D+1):
    for i in range(1,T+1):
        for j in range(1,T+1):
            for k in range(0,2):
                if i == j:
                    model.addConstr(x[(2*T**2)*(l-1)+(2*T)*(i-1)+(2)*(j-1)+(k)]==0,"temp")

# Teams play at most once per day
for l in range(1,D+1):
    for i in range(1,T+1):
        temp_constraint = x[(2*T**2)*(l-1)+(2*T)*(i-1)]
        for j in range(1,T+1):
            for k in range(0,2):
                if not (j == 1 and k == 0):
                    temp_constraint += x[(2*T**2)*(l-1)+(2*T)*(i-1)+(2)*(j-1)+(k)]
        model.addConstr(temp_constraint<=1)

#If team i plays team j on day l at a location k, then team j plays team i on day l at location abs(k-1)
for l in range(1,D+1):
    for i in range(1,T+1):
        for j in range(1,T+1):
            for k in range(0,2):
                if i != j:
                    model.addConstr(x[(2*T**2)*(l-1)+(2*T)*(i-1)+(2)*(j-1)+(k)]==x[(2*T**2)*(l-1)+(2*T)*(j-1)+(2)*(i-1)+(abs(k-1))])

# if team i is at j on day l, and team i is at m on day l+1, then they must travel from j to m on day l
for l in range(0,D):
    for i in range(1,T+1):
        for j in range(1,T+1):
            for m in range(1,T+1):
                model.addConstr(z[(T**2)*(l)+(T)*(i-1)+(j-1)]+
                                z[(T**2)*(l+1)+(T)*(i-1)+(m-1)]-1<=
                                y[(T**3)*(l)+(T**2)*(i-1)+(T)*(j-1)+(m-1)])

# if team i plays j at k, then they must be in i or j
for l in range(1,D+1):
    for i in range(1,T+1):
        for j in range(1,T+1):
            for k in range(0,2):
                if k == 0:
                    model.addConstr(x[(2*T**2)*(l-1)+(2*T)*(i-1)+(2)*(j-1)+(k)]<=z[(T**2)*(l)+(T)*(i-1)+(i-1)])
                if k == 1 and i != j:
                    model.addConstr(x[(2*T**2)*(l-1)+(2*T)*(i-1)+(2)*(j-1)+(k)]==z[(T**2)*(l)+(T)*(i-1)+(j-1)])

# Team i can only be in one place j on day l
for l in range(0,D+1):
    for i in range(1,T+1):
        temp_constraint = z[(T**2)*(l)+(T)*(i-1)]
        for j in range(1,T+1):
            if j != 1:
                temp_constraint += z[(T**2)*(l)+(T)*(i-1)+(j-1)]
        model.addConstr(temp_constraint==1)

#Team i travels only once on day l
for l in range(0,D):
    for i in range(1,T+1):
        temp_constraint = y[(T**3)*(l)+(T**2)*(i-1)]
        for j in range(1,T+1):
            for m in range(1,T+1):
                if not (j == 1 and m == 1):
                    temp_constraint += y[(T**3)*(l)+(T**2)*(i-1)+(T)*(j-1)+(m-1)]
        model.addConstr(temp_constraint<=1)

# Perform epsilon algorithm for integer problems, outputting optimal solutions and values
eps = float('inf')
model.setParam(GRB.Param.Method, 0)
l = 10**-4
scenario_count = 1
while True:
    model.setObjective(l*(objective1)+(1-l)*(objective2), GRB.MINIMIZE)
    model.addConstr(objective1<=eps,"temp")
    model.optimize()
    if model.status != GRB.OPTIMAL:
        break
    else:
        print("SCENARIO: " + str(scenario_count))
        print("")
        for l in range(1,D+1):
            print("Day " + str(l) + " Game Schedule:\n")
            for i in range(1,T+1):
                for j in range(1,T+1):
                    for k in range(0,2):
                        if x[(2*T**2)*(l-1)+(2*T)*(i-1)+(2)*(j-1)+(k)].X == 1:
                            print(team_dictionary[i], end=" ")
                            if k == 0:
                                print("VS. " + team_dictionary[j] + " At: Home")
                            elif k == 1:
                                print("VS. " + team_dictionary[j] + " At: Away")
            print("")

        for l in range(0,D):
            print("Day " + str(l) + " Movement Schedule:\n")
            for i in range(1,T+1):
                for j in range(1,T+1):
                    for k in range(1,T+1):
                        if y[(T**3)*(l)+(T**2)*(i-1)+(T)*(j-1)+(k-1)].X == 1:
                            print("The " + team_dictionary[i] + " Moves From The " + team_dictionary[j] + " Arena To The " + team_dictionary[k] + " Arena")
            print("")
            

        for l in range(0,D+1):
            print("Day " + str(l) + " Team Location:\n")
            for i in range(1,T+1):
                for j in range(1,T+1):
                    if z[(T**2)*(l)+(T)*(i-1)+(j-1)].X == 1:
                        print("The " + team_dictionary[i], end=" ")
                        print("In The City Of The " + team_dictionary[j])
            print("")

        for i in range(1,T+1):
            print("The " + team_dictionary[i] + " Total Back To Backs: ", end="")
            btb = 0
            for l in range(2,D+1):
                if b[(T)*(l-2)+(i-1)].X == 1:
                    btb += 1
            print(btb)
        print("")
        print("Minimum Max Difference Between Team's Total Back to Backs: " + str(objective1.getValue()))
        print("")
        print("Total Distance Traveled By All Teams: " + str(objective2.getValue()))
        print("")
        eps = objective1.getValue()-1
        scenario_count += 1
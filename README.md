# AI-Drone-Routing 🚁

🤖 This Prolog project was created as part of an AI course assignment.  
It solves delivery drone routing problems using uninformed and informed search strategies.

## 📁 Files

- `drone_routing.pl`: The Prolog solution for both problems (uninformed and informed search).
- `AI - Assignment 2 [Spring 2025].pdf`: The official assignment description provided by the course.

📄 **[View Assignment Description](./AI%20-%20Assignment%202%20%5BSpring%202025%5D.pdf)**

## 🧠 Problems Solved

### Problem 1: Uninformed Search (DFS / BFS)

- A drone starts at the top-left corner of a grid.
- It needs to visit as many delivery points (`P`) as possible while avoiding obstacles (`O`).
- Implemented using DFS or BFS.

**Bonus**: Visual representation of the grid before and after pathfinding.

---

### Problem 2: Informed Search (A*)

- The drone must visit **all** delivery points while minimizing total cost.
- Implemented using A* with a heuristic function.
- Path must be the shortest possible to complete all deliveries.

**Bonus**:  
- Energy constraint (e.g., 6 moves max before recharging).  
- Recharge station (`R`) refills the battery.  
- Modified A* includes energy logic.

---

## 🚀 How to Run

1. Open SWI-Prolog or any compatible Prolog interpreter.
2. Load the file:

```prolog
?- consult('drone_routing').
```

## 👥 Team Members

- Israa Mohamed
- Amany Mohamed
- Salma Mohamed
- Rana Ibrahim

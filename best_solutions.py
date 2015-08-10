#!/usr/bin/env python3

import json

def essentials_of(solution):
    result = {}
    for key in ['problemId', 'score', 'solution', 'seed']:
        result[key] = solution[key]
    return result

with open("scores.json") as f:
    best = {}

    for solution in json.loads(f.readline()):
        problem_id = solution['problemId']
        if problem_id not in best:
            best[problem_id] = essentials_of(solution)
        else:
            if best[problem_id]['score'] < solution['score']:
                best[problem_id] = essentials_of(solution)

    for solution in best.values():
        del solution['score']

    print(json.dumps(list(best.values())))

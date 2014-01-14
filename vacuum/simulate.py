class State(object):
    def __init__(self, pos, dirt):
        self.pos = pos
        self.dirt = dirt
        self.score = 0

    def set(self, attr, value):
        s = State(self.pos, self.dirt)
        s.score = self.score
        setattr(s, attr, value)
        return s

def move(state, action):
    assert action in ('LEFT', 'RIGHT', 'SUCK')
    if action in ('LEFT', 'RIGHT'):
        # penalty for movement
        state = state.set('score', state.score - 1)
    if action == 'LEFT':
        return state.set('pos', 'L')
    elif action == 'RIGHT':
        return state.set('pos', 'R')
    elif action == 'SUCK':
        return state.set('dirt', state.dirt.difference({state.pos}))

def sense(state):
    return {'pos' : state.pos,
            'dirt' : state.pos in state.dirt}

def update(state):
    score = state.score + 2 - len(state.dirt)
    return state.set('score', score)

def evaluate(agent, state):
    for _ in range(1000):
        action = agent(sense(state))
        state = move(state, action)
        state = update(state)
    return state.score

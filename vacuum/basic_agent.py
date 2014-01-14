from simulate import evaluate, State

def big_eval(agent):
    total = 0
    for n in range(8):
        pos = 'L' if n & 1 else 'R'
        dirt = set()
        if n & 2:
            dirt.add('L')
        if n & 4:
            dirt.add('R')
        initialstate = State(pos, dirt)
        score = evaluate(agent(), initialstate)
        print ' pos={}, dirt={} -> {}'.format(pos, dirt, score)
        total += score
    print ' average:', total / 8.0
    

def simple_reflex_agent(sensor):
    if sensor['dirt']:
        return 'SUCK'
    elif sensor['pos'] == 'R':
        return 'LEFT'
    elif sensor['pos'] == 'L':
        return 'RIGHT'

print 'simple reflex agent:'
big_eval(lambda: simple_reflex_agent)

class state_reflex_agent(object):
    def __init__(self):
        self.dirty = {'L', 'R'}
    def __call__(self, sensor):
        self.dirty.discard(sensor['pos'])
        if sensor['dirt']:
            return 'SUCK'
        elif sensor['pos'] == 'R' and ('L' in self.dirty):
            return 'LEFT'
        elif sensor['pos'] == 'L' and ('R' in self.dirty):
            return 'RIGHT'
        else:
            return 'SUCK'

print 'state reflex agent:'
big_eval(state_reflex_agent)

from functools import partial
import fbuild.db

@fbuild.db.caches
def run(ctx, src: fbuild.db.SRC) -> fbuild.db.DST:
    dst = src.replace('in', 'out')
    ctx.execute(['sh', 'parallel-run', src, '--', dst], 'parallel-run')
    return dst

def build(ctx):
    ctx.scheduler.map(partial(run, ctx), ['input1', 'input2'])

from fbuild.target import register
import fbuild.db

@fbuild.db.caches
def run(ctx, srcs: fbuild.db.SRCS, dst: fbuild.db.DST):
    ctx.execute('cat {} > {}'.format(' '.join(srcs[1:]), dst), 'cat', shell=True)

@register()
def output(ctx):
    with open('list') as f:
        lines = f.read().splitlines()
    run(ctx, ['list']+lines, 'output')

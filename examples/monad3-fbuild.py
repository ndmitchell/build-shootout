from fbuild.target import register
import fbuild.db

@fbuild.db.caches
def make_list(ctx, src: fbuild.db.SRC, dst: fbuild.db.DST):
    ctx.execute(['sh', 'monad3-run', src, '--', dst], 'monad3-run')

@fbuild.db.caches
def gen(ctx, dst: fbuild.db.DST):
    ctx.execute(['sh', 'monad3-gen', '--', dst], 'monad3-gen')

@fbuild.db.caches
def run(ctx, srcs: fbuild.db.SRCS, dst: fbuild.db.DST):
    ctx.execute('cat {} > {}'.format(' '.join(srcs[1:]), dst), 'cat', shell=True)

genmap = {'gen': gen}

@register()
def output(ctx):
    make_list(ctx, 'source', 'list')
    with open('list') as f:
        lines = f.read().splitlines()
    for dep in lines:
        genmap.get(dep, lambda *_: None)(ctx, dep)
    run(ctx, ['list']+lines, 'output')

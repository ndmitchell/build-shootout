from fbuild.target import register
import fbuild.db

def gen(ctx, dst) -> fbuild.db.DST:
    ctx.execute(['sh', 'system1-gen', '--', dst], 'system1-gen')
    return dst

@fbuild.db.caches
def run(ctx, src: fbuild.db.SRC, dst: fbuild.db.DST):
    ctx.execute(['sh', 'system1-run', src, '--', dst], 'system1-run')

@register()
def output(ctx):
    run(ctx, gen(ctx, 'source'), 'output')

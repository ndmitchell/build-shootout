from fbuild.target import register
import fbuild.db

@fbuild.db.caches
def gen(ctx, src: fbuild.db.SRC, dst: fbuild.db.DST):
    ctx.execute(['sh', 'unchanged-gen', src, '--', dst], 'unchanged-gen')

@fbuild.db.caches
def run(ctx, src: fbuild.db.SRC, dst: fbuild.db.DST):
    ctx.execute(['sh', 'unchanged-run', src, '--', dst], 'unchanged-run')

@register()
def output(ctx):
    gen(ctx, 'input', 'source')
    run(ctx, 'source', 'output')

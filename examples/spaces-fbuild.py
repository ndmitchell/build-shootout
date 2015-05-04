from fbuild.target import register
import fbuild.db

@fbuild.db.caches
def run(ctx, src: fbuild.db.SRC, dst: fbuild.db.DST):
    ctx.execute(['cp', src, dst], 'cp')

@register(name='output file')
def build(ctx):
    run(ctx, 'input file', 'output file')

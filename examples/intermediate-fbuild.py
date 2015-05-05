from fbuild.path import Path
import fbuild.db

@fbuild.db.caches
def run(ctx, src: fbuild.db.SRC, dst: fbuild.db.DST):
    i = Path('intermediate')
    ctx.execute(['sh', 'intermediate-run', src, '--', i], 'intermediate-run')
    ctx.execute(['sh', 'intermediate-run', i, '--', dst], 'intermediate-run')
    i.remove()

def build(ctx):
    run(ctx, 'input', 'output')

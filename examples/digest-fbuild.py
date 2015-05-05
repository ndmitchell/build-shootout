import fbuild.db

@fbuild.db.caches
def run(ctx, src: fbuild.db.SRC, dst: fbuild.db.DST):
    ctx.execute(['sh', 'digest-run', src, '--', dst], 'digest-run')

def build(ctx):
    run(ctx, 'input', 'output')

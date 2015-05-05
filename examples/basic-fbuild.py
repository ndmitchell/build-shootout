import fbuild.db

@fbuild.db.caches
def run(ctx, src: fbuild.db.SRC, dst: fbuild.db.DST):
    ctx.execute(['sh', 'basic-run', src, '--', dst], 'basic-run')

def build(ctx):
    run(ctx, 'input', 'output')

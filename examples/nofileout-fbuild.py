import fbuild.db

@fbuild.db.caches
def run(ctx, src: fbuild.db.SRC):
    ctx.execute(['sh', 'nofileout-run', src], 'nofileout-run')

def build(ctx):
    run(ctx, 'input')

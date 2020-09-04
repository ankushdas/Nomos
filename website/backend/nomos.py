# This script works with pyton 3.8
# Warning: resource limits need to be enforced by AWS Lambda

import os
import subprocess
import json

# lambda root is the location of the application (content of the uploaded zip)
lambda_root = os.environ['LAMBDA_TASK_ROOT']

# location of the analysis tool
nomos_bin = lambda_root + '/bin/nomosjson'

# location of the clp libraries
ld_path = lambda_root + '/lib/'

#execute nomos
def run_nomos(json_input):
        # set shell environment
        my_env = os.environ
        my_env['LD_LIBRARY_PATH'] = ld_path

        # run the analysis
        try:
                nomos_output = subprocess.check_output (
		                [nomos_bin],
                                input=json_input,
                                test=True)
        except subprocess.CalledProcessError as e:
                nomos_output = e.output

	# return the result
        return nomos_output

def main(event, context):
        json_input = json.dumps(event)

        #run Nomos
        nomos_output = run_nomos(json_input)

        return (json.loads(nomos_output))

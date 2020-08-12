# This script works with pyton 3.8
# Warning: resource limits need to be enforced by AWS Lambda

import sys
import os
import subprocess
import tempfile

# location of the analysis tool
# lambda root is the location of the application (content of the uploaded zip)
lambda_root = os.environ['LAMBDA_TASK_ROOT']
nomos_bin = lambda_root + '/bin/nomos'

# location of the clp libraries
ld_path = lambda_root + '/lib/'

#execute nomos
def run_nomos(event, tmp):
        # set shell environment
        my_env = os.environ
        my_env['LD_LIBRARY_PATH'] = ld_path

        # if event['mode'] == 'eval':
        #         raml_options = ['eval']
        # else:
        #         amode = event['amode']
        #         metric = event['metric']
        #         degree = event['degree']
        #         if event['mode'] == 'module':
        #                 module_mode = ['-m']
        #         else:
        #                 module_mode = []
        #         raml_options = ['analyze', amode, metric, degree, '-print', 'level', '2'] + module_mode
                
        # run the analysis
        # hack: we need to run raml in lambda_root directory so that we can find the runtime files
        try:
                nomos_output = subprocess.check_output (
		                [nomos_bin]  + ["-tc"] + [tmp.name],
                                cwd=lambda_root,
                                stderr = subprocess.STDOUT )
        except subprocess.CalledProcessError as e:
                nomos_output = e.output

	# return the result
        return nomos_output

def main(event, context):
        #create a temporary file with the code from the form
        tmp = tempfile.NamedTemporaryFile(suffix='.nom')
        code = event['code']
        tmp.write(code.encode('utf-8')) #not clear if the format really is utf-8
        tmp.flush()
        os.fsync(tmp.fileno())

        #run Nomos
        nomos_output = run_nomos(event, tmp)

        # close the temp file
        tmp.close()

        return nomos_output

# Testing

# test_form = {"code" : "3", "mode" : "analyze", "amode" : "upper", "metric":"steps", "degree":"2"}

# test_form = {"code" : "3", "mode" : "analyze", "amode" : "upper", "metric":"steps", "degree":"2"}


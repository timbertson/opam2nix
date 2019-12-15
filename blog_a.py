import os, sys

echo_r, echo_w = os.pipe()
pid = os.fork()
if pid == 0:
	# write to stdout
	os.close(echo_r)
	print('spawn echo')
	os.dup2(echo_w,1) # make the writeable end of the pipe our `stdout`
	os.execvp('echo', ['echo', '-e', r'1\n2\n3'])
else:
	os.close(echo_w)
	# pass echo_r (the output of echo) into `sed`:
	sed_r, sed_w = os.pipe()
	pid = os.fork()
	if pid == 0:
		os.close(sed_r)
		print('spawn sed')
		os.dup2(sed_w, 1)
		os.dup2(echo_r, 0)
		os.execvp('sed', ['sed', '-e', 's/$/! ah hah ha!/'])
	else:
		os.close(echo_r)
		os.close(sed_w)
		while True:
			chunk = os.read(sed_r, 100)
			if chunk == b'':
				print("read EOF")
				break
			print("read chunk: " + chunk.decode('utf-8'))

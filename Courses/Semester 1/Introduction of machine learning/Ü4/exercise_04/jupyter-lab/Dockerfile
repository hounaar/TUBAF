FROM jupyter/tensorflow-notebook
RUN jupyter server --generate-config
RUN sed -i -- "s/# c.ServerApp.password = ''/c.ServerApp.password = 'argon2:\$argon2id\$v=19\$m=10240,t=10,p=8\$ybRNT0IODFEAs2XY9OpWNg\$1SkSButfUryWxq01QRWxybqQPGqvP\/xF2Xnl9XUz5NI'/g" .jupyter/jupyter_server_config.py
RUN pip install pip --upgrade && pip install 'jupyterlab-lsp' 'python-lsp-server[all]'

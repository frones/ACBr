clear
_rst:=''
_local_:='acbrlib.ini'
keyboard(1)
cep := ACBrCEP():New(_local_, '')
cep:ConfigGravarValor("Principal", "LogNivel", "4")
cep:ConfigGravarValor("CEP", "WebService", "11") //1 ou 11 
cep:ConfigGravarValor("CEP", "ChaveAcesso", "")
cep:ConfigGravarValor("CEP", "Usuario", "")
cep:ConfigGravarValor("CEP", "Senha", "")
cep:ConfigGravarValor("CEP", "PesquisarIBGE", "0")
_rst := cep:BuscarPorCEP("22735020")
alert(_rst)

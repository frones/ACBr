import ctypes
                        # AQUI VC COLOCAR CAMINHO DA DLL
acbr_lib = ctypes.CDLL(r'C:\ACBr\Projetos\ACBrLib\Demos\Python\PosPrinter\ACBrPosPrinter64.dll')
                        # AQUI VC COLOCAR CAMINHO DO ARQUIVO INI
resultado = acbr_lib.POS_Inicializar(r'C:\ACBr\Projetos\ACBrLib\Demos\Python\PosPrinter\ACBrLIB.ini'.encode("utf-8"), "".encode("utf-8"))

#tratar uma sequencia de bytes     
def TrataRespostaLerValor(AResposta):
    return AResposta.decode('utf-8').strip('\x00 ')

print('Inicializar Codigo Resposta ',resultado)

texto = f"""</ce><qrcode>www.google.com</qrcode>
OLÁ ESTA É UMA MENSAGEM DE TESTE


COMPREENDIDO ESTOU TESTANDO!









"""
resultado = acbr_lib.POS_Ativar()
print('Ativar Codigo Resposta ',resultado)
eString = ctypes.create_string_buffer(len(texto))
ctypes.memmove(eString, texto.encode('utf-8'), len(texto))


Copias = 1

acbr_lib.POS_InicializarPos()

sResposta = ctypes.create_string_buffer(1024) 
esTamanho = ctypes.c_int(1024)


retorno = acbr_lib.POS_LerStatusImpressora('1'.encode("utf-8"), sResposta)
print('retorno LerStatusImpressora ',retorno)
print('Status ', sResposta)

#Lretorno = TrataRespostaLerValor(sResposta)

#print('Status ', Lretorno)

#acbr_lib.POS_Imprimir(eString, True, True, True, Copias)

#acbr_lib.POS_CortarPapel(False)

acbr_lib.POS_Desativar()

acbr_lib.POS_Finalizar()

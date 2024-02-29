import ctypes
                        # AQUI VC COLOCAR CAMINHO DA DLL
acbr_lib = ctypes.CDLL(r'C:\ACBrLibPosPrinterPython\ACBrPosPrinter64.dll')
                        # AQUI VC COLOCAR CAMINHO DO ARQUIVO INI
acbr_lib.POS_Inicializar(r'C:\ACBrLibPosPrinterPython\ACBrLIB.ini'.encode("utf-8"), "".encode("utf-8"))

texto = f"""</ce><qrcode>www.google.com</qrcode>
OLÁ ESTA É UMA MENSAGEM DE TESTE


COMPREENDIDO ESTOU TESTANDO!









"""
acbr_lib.POS_Ativar()

eString = ctypes.create_string_buffer(len(texto))
ctypes.memmove(eString, texto.encode('utf-8'), len(texto))

Copias = 1

acbr_lib.POS_InicializarPos()

acbr_lib.POS_Imprimir(eString, True, True, True, Copias)

acbr_lib.POS_CortarPapel(False)

acbr_lib.POS_Desativar()

acbr_lib.POS_Finalizar()

import ctypes

acbr_lib = ctypes.CDLL('.\DLL\ACBrBAL64.dll')
acbr_lib.BAL_Inicializar('.\ACBRrINI.ini'.encode("utf-8"), "".encode("utf-8"))
# VOCE CONFIGURAR NO INI "Porta" | "Modelo" | "Baud"
acbr_lib.BAL_Ativar()

MillisecTimeOut = ctypes.c_int(1000)
Peso = ctypes.create_string_buffer(1024)

acbr_lib.BAL_SolicitarPeso()

sValor = ctypes.create_string_buffer(1024)
while sValor.value.decode("utf-8") == "":
    funcao = acbr_lib.BAL_LePesoStr(MillisecTimeOut, Peso)
    if funcao == 0:
        #sValor = ctypes.create_string_buffer(1024)
        acbr_lib.BAL_UltimoPesoLidoStr(sValor)
        peso = sValor.value.decode("utf-8")
        if peso != "":
            print(f"O Peso E: {peso}")
    print("VERIFICANDO")

acbr_lib.BAL_Desativar()
acbr_lib.BAL_Finalizar()

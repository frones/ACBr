import ctypes

while True:
    print("Menu:")
    print("1. cwsBrasilAPI")
    print("2. cwsReceitaWS")
    print("Digite 0 para sair")

    try:
        provedor = int(input("Escolha uma opção: "))

        if provedor == 1:
            print("Você escolheu a Opção 1.")
        elif provedor == 2:
            print("Você escolheu a Opção 2.")
        else:
            print("Encerrando o programa.")
            break

        # Carregar a DLL
        acbr_lib = ctypes.CDLL(r'C:\ACBrLibConsultaCNPJPython\ACBrConsultaCNPJ64.dll')
        inicializa = acbr_lib.CNPJ_Inicializar(r'C:\ACBrLibConsultaCNPJPython\ACBrConsultaCNPJ.INI'.encode("utf-8"),"".encode("utf-8"))

        # Definir a assinatura da função
        acbr_lib.CNPJ_Consultar.argtypes = (ctypes.c_char_p, ctypes.c_int, ctypes.c_char_p, ctypes.POINTER(ctypes.c_int))
        acbr_lib.CNPJ_Consultar.restype = ctypes.c_int

        # Definir variáveis para armazenar os resultados
        sResposta = ctypes.create_string_buffer(1024) 
        esTamanho = ctypes.c_int(1024)
        
        # Definir o valor do CNPJ como uma string
        cnpj_valor =  input("Digite o CNPJ (somente numeros): ")

        # Criar um buffer de string com espaço suficiente
        sCNPJ = ctypes.create_string_buffer(15)  # +1 para o caractere nulo de terminação

        # Copiar o valor do CNPJ para o buffer
        ctypes.memmove(sCNPJ, cnpj_valor.encode('utf-8'), len(cnpj_valor))

        # Chamar a função CNPJ_Versao
        resultado = acbr_lib.CNPJ_Consultar(sCNPJ,provedor,sResposta,ctypes.byref(esTamanho))

        # Verificar o resultado
        if resultado == 0:
            print(f"tamanho da resposta: {esTamanho}")
            sMensagem = ctypes.create_string_buffer(534 + 1)    
            acbr_lib.CNPJ_UltimoRetorno(sMensagem,ctypes.byref(esTamanho))
            print(sMensagem.value.decode('utf-8'))
        else:
            print(f"Falha ao consultar CNPJ. Código de erro: {resultado}")

        acbr_lib.CNPJ_Finalizar()


    except ValueError:
        print("Entrada inválida. Por favor, digite um número.")




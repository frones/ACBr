package com.acbr.etq;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
import java.io.File;
import java.nio.ByteBuffer;
import java.nio.file.Paths;

public final class ACBrETQ extends ACBrLibBase implements AutoCloseable  {
    
    interface ACBrETQLib extends Library {

        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrETQLib INSTANCE = LibraryLoader.getInstance();

        static class LibraryLoader {

            private static String library = "";
            private static ACBrETQLib instance = null;

            public static String getLibraryName() {
                if (library.isEmpty()) {
                    library = Platform.is64Bit() ? "ACBrETQ64" : "ACBrETQ32";
                }
                return library;
            }

            public static ACBrETQLib getInstance() {
                if (instance == null) {
                    instance = (ACBrETQLib) Native.synchronizedLibrary((Library) Native.loadLibrary(JNA_LIBRARY_NAME, ACBrETQLib.class));
                }
                return instance;
            }
        }

        int ETQ_Inicializar(String eArqConfig, String eChaveCrypt);

        int ETQ_Finalizar();

        int ETQ_Nome(ByteBuffer buffer, IntByReference bufferSize);

        int ETQ_Versao(ByteBuffer buffer, IntByReference bufferSize);

        int ETQ_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

        int ETQ_ConfigLer(String eArqConfig);

        int ETQ_ConfigGravar(String eArqConfig);

        int ETQ_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int ETQ_ConfigGravarValor(String eSessao, String eChave, String valor);

        int ETQ_Ativar();

        int ETQ_Desativar();

        int ETQ_IniciarEtiqueta();

        int ETQ_FinalizarEtiqueta(int ACopias, int AAvancoEtq);

        int ETQ_CarregarImagem(String eArquivoImagem, String eNomeImagem, boolean Flipped);

        int ETQ_Imprimir(int ACopias, int AAvancoEtq);

        int ETQ_ImprimirTexto(int Orientacao, int Fonte, int MultiplicadorH, int MultiplicadorV,
                int Vertical, int Horizontal, String eTexto, int SubFonte, boolean ImprimirReverso);

        int ETQ_ImprimirTextoStr(int Orientacao, String Fonte, int MultiplicadorH, int MultiplicadorV,
                int Vertical, int Horizontal, String eTexto, int SubFonte, boolean ImprimirReverso);

        int ETQ_ImprimirBarras(int Orientacao, int TipoBarras, int LarguraBarraLarga, int LarguraBarraFina,
                int Vertical, int Horizontal, String eTexto, int AlturaCodBarras, int ExibeCodigo);

        int ETQ_ImprimirLinha(int Vertical, int Horizontal, int Largura, int Altura);

        int ETQ_ImprimirCaixa(int Vertical, int Horizontal, int Largura, int Altura, int EspessuraVertical,
                int EspessuraHorizontal);

        int ETQ_ImprimirImagem(int MultiplicadorImagem, int Vertical, int Horizontal, String eNomeImagem);
    }

    public ACBrETQ() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        int ret = ACBrETQLib.INSTANCE.ETQ_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrETQ(String eArqConfig, String eChaveCrypt) throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }
    
    @Override
    public void close() throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_Finalizar();
        checkResult(ret);
    }

    @Override
    protected void finalize() throws Throwable {
        try {
            int ret = ACBrETQLib.INSTANCE.ETQ_Finalizar();
            checkResult(ret);
        } finally {
            super.finalize();
        }
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrETQLib.INSTANCE.ETQ_Nome(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrETQLib.INSTANCE.ETQ_Versao(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrETQLib.INSTANCE.ETQ_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }

    public void ativar() throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_Ativar();
        checkResult(ret);
    }

    public void desativar() throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_Desativar();
        checkResult(ret);
    }

    public void iniciarEtiqueta() throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_IniciarEtiqueta();
        checkResult(ret);
    }

    public void finalizarEtiqueta() throws Exception {
        finalizarEtiqueta(1, 0);
    }

    public void finalizarEtiqueta(int aCopias, int aAvancoEtq) throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_FinalizarEtiqueta(aCopias, aAvancoEtq);
        checkResult(ret);
    }

    public void carregarImagem(String eArquivoImagem, String eNomeImagem) throws Exception {
        carregarImagem(eArquivoImagem, eNomeImagem, true);
    }

    public void carregarImagem(String eArquivoImagem, String eNomeImagem, boolean flipped) throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_CarregarImagem(toUTF8(eArquivoImagem), toUTF8(eNomeImagem), flipped);
        checkResult(ret);
    }

    public void imprimir() throws Exception {
        imprimir(1, 0);
    }

    public void imprimir(int aCopias, int aAvancoEtq) throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_Imprimir(aCopias, aAvancoEtq);
        checkResult(ret);
    }

    public void imprimirTexto(ETQOrientacao orientacao, int fonte, int multiplicadorH, int multiplicadorV,
            int vertical, int horizontal, String eTexto) throws Exception {
        imprimirTexto(orientacao, fonte, multiplicadorH, multiplicadorV, vertical, horizontal, eTexto, 0, false);
    }

    public void imprimirTexto(ETQOrientacao orientacao, int fonte, int multiplicadorH, int multiplicadorV,
            int vertical, int horizontal, String eTexto, int subFonte, boolean imprimirReverso) throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_ImprimirTexto(orientacao.asInt(), fonte, multiplicadorH, multiplicadorV,
                vertical, horizontal, toUTF8(eTexto), subFonte, imprimirReverso);
        checkResult(ret);
    }

    public void imprimirTexto(ETQOrientacao orientacao, String fonte, int multiplicadorH, int multiplicadorV,
            int vertical, int horizontal, String eTexto) throws Exception {
        imprimirTexto(orientacao, fonte, multiplicadorH, multiplicadorV, vertical, horizontal, eTexto, 0, false);
    }

    public void imprimirTexto(ETQOrientacao orientacao, String fonte, int multiplicadorH, int multiplicadorV,
            int vertical, int horizontal, String eTexto, int subFonte, boolean imprimirReverso) throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_ImprimirTextoStr(orientacao.asInt(), toUTF8(fonte), multiplicadorH, multiplicadorV,
                vertical, horizontal, toUTF8(eTexto), subFonte, imprimirReverso);
        checkResult(ret);
    }

    public void imprimirBarras(ETQOrientacao orientacao, TipoCodBarra tipoBarras, int larguraBarraLarga, int larguraBarraFina,
            int vertical, int horizontal, String eTexto) throws Exception {
        imprimirBarras(orientacao, tipoBarras, larguraBarraLarga, larguraBarraFina, vertical, horizontal, eTexto, 0, ETQBarraExibeCodigo.becPadrao);
    }

    public void imprimirBarras(ETQOrientacao orientacao, TipoCodBarra tipoBarras, int larguraBarraLarga, int larguraBarraFina,
            int vertical, int horizontal, String eTexto, int alturaCodBarras, ETQBarraExibeCodigo exibeCodigo) throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_ImprimirBarras(orientacao.asInt(), tipoBarras.asInt(), larguraBarraLarga, larguraBarraFina,
                vertical, horizontal, toUTF8(eTexto), alturaCodBarras, exibeCodigo.asInt());
        checkResult(ret);
    }

    public void imprimirLinha(int vertical, int horizontal, int largura, int altura) throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_ImprimirLinha(vertical, horizontal, largura, altura);
        checkResult(ret);
    }

    public void imprimirCaixa(int vertical, int horizontal, int largura, int altura, int espessuraVertical,
            int espessuraHorizontal) throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_ImprimirCaixa(vertical, horizontal, largura, altura, espessuraVertical, espessuraHorizontal);
        checkResult(ret);
    }

    public void imprimirImagem(int multiplicadorImagem, int vertical, int horizontal, String eNomeImagem) throws Exception {
        int ret = ACBrETQLib.INSTANCE.ETQ_ImprimirImagem(multiplicadorImagem, vertical, horizontal, toUTF8(eNomeImagem));
        checkResult(ret);
    }

    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrETQLib.INSTANCE.ETQ_UltimoRetorno(buffer, bufferLen);
    }
}

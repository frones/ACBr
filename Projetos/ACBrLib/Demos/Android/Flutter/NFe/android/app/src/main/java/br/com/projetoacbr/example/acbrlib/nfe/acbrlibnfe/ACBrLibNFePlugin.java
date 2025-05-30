package br.com.projetoacbr.example.acbrlib.nfe.acbrlibnfe;

import androidx.annotation.NonNull;

import java.io.File;
import android.content.Context;

import br.com.acbr.lib.nfe.ACBrLibNFe;
import io.flutter.embedding.engine.plugins.FlutterPlugin;
import io.flutter.plugin.common.MethodCall;
import io.flutter.plugin.common.MethodChannel;

public class ACBrLibNFePlugin implements FlutterPlugin, MethodChannel.MethodCallHandler {
    private MethodChannel channel;
    private ACBrLibNFe ACBrNFe;
    private String arquivoConfig;
    private File appDir;

    @Override
    public void onAttachedToEngine(@NonNull FlutterPluginBinding binding){
        Context context = binding.getApplicationContext();
        appDir = (File) context.getExternalFilesDir(null);
        arquivoConfig =  appDir.getAbsolutePath() + "/acbrlib.ini";

        channel = new MethodChannel(binding.getBinaryMessenger(), "br.com.projetoacbr.example.acbrlib.nfe.acbrlibnfe");
        channel.setMethodCallHandler(this);
        ACBrNFe = new ACBrLibNFe(arquivoConfig, "");
    }

    @Override
    public void onDetachedFromEngine(@NonNull FlutterPluginBinding binding){
        channel.setMethodCallHandler(null);
        try{
            ACBrNFe.finalizar();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            channel.setMethodCallHandler(null);
        }
    }

    @Override
    public void onMethodCall(@NonNull MethodCall call, @NonNull MethodChannel.Result result) {

        switch (call.method) {
            case "inicializar":
                handleInicializar(result);
                break;
            case "finalizar":
                handleFinalizar(result);
                break;
            case "ultimoRetorno":
                handleUltimoRetorno(result);
                break;
            case "nome":
                handleNome(result);
                break;
            case "versao":
                handleVersao(result);
                break;
            case "configLer":
                handleConfigLer(call, result);
                break;
            case "configGravar":
                handleConfigGravar(result);
                break;
            case "configLerValor":
                handleConfigLerValor(call, result);
                break;
            case "configGravarValor":
                handleConfigGravarValor(call, result);
                break;
            case "configImportar":
                handleConfigImportar(call, result);
                break;
            case "configExportar":
                handleConfigExportar(result);
                break;
            case "openSSLInfo":
                handleOpenSSLInfo(result);
                break;
            case "carregarXML":
                handleCarregarXML(call, result);
                break;
            case "carregarINI":
                handleCarregarINI(call, result);
                break;
            case "obterXML":
                handleObterXML(call, result);
                break;
            case "gravarXML":
                handleGravarXML(call, result);
                break;
            case "obterINI":
                handleObterINI(call, result);
                break;
            case "gravarINI":
                handleGravarINI(call, result);
                break;
            case "carregarEventoXML":
                handleCarregarEventoXML(call, result);
                break;
            case "carregarEventoINI":
                handleCarregarEventoINI(call, result);
                break;
            case "limparLista":
                handleLimparLista(result);
                break;
            case "limparListaEventos":
                handleLimparListaEventos(result);
                break;
            case "assinar":
                handleAssinar(result);
                break;
            case "validar":
                handleValidar(result);
                break;
            case "validarRegrasdeNegocios":
                handleValidarRegrasdeNegocios(result);
                break;
            case "verificarAssinatura":
                handleVerificarAssinatura(result);
                break;
            case "gerarChave":
                handleGerarChave(call, result);
                break;
            case "obterCertificados":
                handleObterCertificados(result);
                break;
            case "getPath":
                handleGetPath(call, result);
                break;
            case "getPathEvento":
                handleGetPathEvento(call, result);
                break;
            case "statusServico":
                handleStatusServico(result);
                break;
            case "consultar":
                handleConsultar(call, result);
                break;
            case "consultarRecibo":
                handleConsultarRecibo(call, result);
                break;
            case "consultaCadastro":
                handleConsultaCadastro(call, result);
                break;
            case "inutilizar":
                handleInutilizar(call, result);
                break;
            case "enviar":
                handleEnviar(call, result);
                break;
            case "cancelar":
                handleCancelar(call, result);
                break;
            case "enviarEvento":
                handleEnviarEvento(call, result);
                break;
            case "distribuicaoDFe":
                handleDistribuicaoDFe(call, result);
                break;
            case "distribuicaoDFePorUltNSU":
                handleDistribuicaoDFePorUltNSU(call, result);
                break;
            case "distribuicaoDFePorNSU":
                handleDistribuicaoDFePorNSU(call, result);
                break;
            case "distribuicaoDFePorChave":
                handleDistribuicaoDFePorChave(call, result);
                break;
            case "enviarEmail":
                handleEnviarEmail(call, result);
                break;
            case "enviarEmailEvento":
                handleEnviarEmailEvento(call, result);
                break;
            case "imprimir":
                handleImprimir(call, result);
                break;
            case "imprimirPDF":
                handleImprimirPDF(call, result);
                break;
            case "salvarPDF":
                handleSalvarPDF(call, result);
                break;
            case "imprimirEvento":
                handleImprimirEvento(call, result);
                break;
            case "imprimirEventoPDF":
                handleImprimirEventoPDF(call, result);
                break;
            case "salvarEventoPDF":
                handleSalvarEventoPDF(call, result);
                break;
            case "imprimirInutilizacao":
                handleImprimirInutilizacao(call, result);
                break;
            case "imprimirInutilizacaoPDF":
                handleImprimirInutilizacaoPDF(call, result);
                break;
            case "salvarInutilizacaoPDF":
                handleSalvarInutilizacaoPDF(call, result);
                break;
            default:
                result.notImplemented();
                break;
        }
    }

    private void handleInicializar(MethodChannel.Result result) {
        int status = -1;
        try {
            status = ACBrNFe.inicializar();
            aplicarConfiguracoesPadrao();
            result.success(status);
        } catch (Exception e) {
            result.error("Erro ao inicializar ACBrLibBAL", e.getMessage(), e);
        }
    }

    private void handleFinalizar(MethodChannel.Result result) {
        try {
            int status = ACBrNFe.finalizar();
            result.success(status);
        } catch (Exception e) {
            result.error("Erro ao finalizar ACBrLibBAL", e.getMessage(), e);
        }
    }

    private void handleUltimoRetorno(MethodChannel.Result result) {
        try {
            String valor = ACBrNFe.ultimoRetorno();
            result.success(valor);
        } catch (Exception e) {
            result.error("Erro ao obter último retorno", e.getMessage(), e);
        }
    }

    private void handleNome(MethodChannel.Result result) {
        try {
            String valor = ACBrNFe.nome();
            result.success(valor);
        } catch (Exception e) {
            result.error("Erro ao obter nome", e.getMessage(), e);
        }
    }

    private void handleVersao(MethodChannel.Result result) {
        try {
            String valor = ACBrNFe.versao();
            result.success(valor);
        } catch (Exception e) {
            result.error("Erro ao obter versão", e.getMessage(), e);
        }
    }

    private void handleConfigLer(MethodCall call, MethodChannel.Result result) {
        try {
            int status = ACBrNFe.configLer(call.argument("eArqConfig"));
            result.success(status);
        } catch (Exception e) {
            result.error("Erro ao ler configuração", e.getMessage(), e);
        }
    }

    private void handleConfigGravar(MethodChannel.Result result) {
        try {
            int status = ACBrNFe.configGravar();
            result.success(status);
        } catch (Exception e) {
            result.error("Erro ao gravar configuração", e.getMessage(), e);
        }
    }

    private void handleConfigLerValor(MethodCall call, MethodChannel.Result result) {
        try {
            String valor = ACBrNFe.configLerValor(call.argument("sessao"), call.argument("chave"));
            result.success(valor);
        } catch (Exception e) {
            result.error("Erro ao ler valor de configuração", e.getMessage(), e);
        }
    }

    private void handleConfigGravarValor(MethodCall call, MethodChannel.Result result){
        try{
            int valor = ACBrNFe.configGravarValor(call.argument("sessao"), call.argument("chave"), call.argument("valor"));
            result.success(valor);
        } catch (Exception e) {
            result.error("Erro ao gravar valor de configuração", e.getMessage(), e);
        }
    }

    private void handleConfigImportar(MethodCall call, MethodChannel.Result result) {
        try {
            int status = ACBrNFe.configImportar(call.argument(arquivoConfig));
            result.success(status);
        } catch (Exception e) {
            result.error("Erro ao importar configuração", e.getMessage(), e);
        }
    }

    private void handleConfigExportar(MethodChannel.Result result) {
        try {
            String valor = ACBrNFe.configExportar();
            result.success(valor);
        } catch (Exception e) {
            result.error("Erro ao exportar configuração", e.getMessage(), e);
        }
    }

    private void handleOpenSSLInfo(MethodChannel.Result result) {
        try {
            String valor = ACBrNFe.openSslInfo();
            result.success(valor);
        } catch (Exception e) {
            result.error("Erro ao obter informações do OpenSSL", e.getMessage(), e);
        }
    }

    private void handleCarregarXML(MethodCall call, MethodChannel.Result result) {
        String eArquivoOuXML = call.argument("eArquivoOuXML");
        try {
            ACBrNFe.CarregarXML(eArquivoOuXML);
            result.success("XML carregado com sucesso");
        } catch (Exception e) {
            result.error("Erro ao carregar XML", e.getMessage(), e);
        }
    }

    private void handleCarregarINI(MethodCall call, MethodChannel.Result result) {
        String eArquivoOuINI = call.argument("eArquivoOuINI");
        try {
            ACBrNFe.CarregarINI(eArquivoOuINI);
            result.success("INI carregado com sucesso");
        } catch (Exception e) {
            result.error("Erro ao carregar INI", e.getMessage(), e);
        }
    }

    private void handleObterXML(@NonNull MethodCall call, @NonNull MethodChannel.Result result) {
        Integer indexArg = call.argument("AIndex");
        if (indexArg == null) {
            result.error("Argumento inválido", "AIndex não pode ser nulo", null);
            return;
        }

        int AIndex = indexArg;
        try {
            String retorno = ACBrNFe.ObterXml(AIndex);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao obter XML", e.getMessage(), e);
        }
    }

    private void handleGravarXML(MethodCall call, MethodChannel.Result result) {
        Integer indexArg = call.argument("AIndex");
        if (indexArg == null) {
            result.error("Argumento inválido", "AIndex não pode ser nulo", null);
            return;
        }
        int AIndex = indexArg;
        String eNomeArquivo = call.argument("eNomeArquivo");
        String ePathArquivo = call.argument("ePathArquivo");

        try {
            ACBrNFe.GravarXml(AIndex, eNomeArquivo, ePathArquivo);
            result.success("XML gravado com sucesso");
        } catch (Exception e) {
            result.error("Erro ao gravar XML", e.getMessage(), e);
        }
    }

    private void handleObterINI(MethodCall call, MethodChannel.Result result){
        Integer indexArg = call.argument("AIndex");
        if (indexArg == null) {
            result.error("Argumento inválido", "AIndex não pode ser nulo", null);
            return;
        }
        int AIndex = indexArg;

        try {
            String retorno = ACBrNFe.ObterIni(AIndex);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao obter INI", e.getMessage(), e);
        }
    }

    private void handleGravarINI(MethodCall call, MethodChannel.Result result) {
        Integer indexArg = call.argument("AIndex");
        if (indexArg == null) {
            result.error("Argumento inválido", "AIndex não pode ser nulo", null);
            return;
        }
        int AIndex = indexArg;
        String eNomeArquivo = call.argument("eNomeArquivo");
        String ePathArquivo = call.argument("ePathArquivo");

        try {
            ACBrNFe.GravarIni(AIndex, eNomeArquivo, ePathArquivo);
            result.success("XML gravado com sucesso");
        } catch (Exception e) {
            result.error("Erro ao gravar XML", e.getMessage(), e);
        }
    }

    private void handleCarregarEventoXML(MethodCall call, MethodChannel.Result result){
        String eArquivoOuXML = call.argument("eArquivoOuXML");
        try {
            ACBrNFe.CarregarEventoXML(eArquivoOuXML);
            result.success("Evento XML carregado com sucesso");
        } catch (Exception e) {
            result.error("Erro ao carregar evento XML", e.getMessage(), e);
        }
    }

    private void handleCarregarEventoINI(MethodCall call, MethodChannel.Result result){
        String eArquivoOuINI = call.argument("eArquivoOuINI");
        try {
            ACBrNFe.CarregarEventoINI(eArquivoOuINI);
            result.success("Evento INI carregado com sucesso");
        } catch (Exception e) {
            result.error("Erro ao carregar evento INI", e.getMessage(), e);
        }
    }

    private void handleLimparLista(MethodChannel.Result result) {
        try {
            ACBrNFe.LimparLista();
            result.success("Lista limpa com sucesso");
        } catch (Exception e) {
            result.error("Erro ao limpar lista", e.getMessage(), e);
        }
    }

    private void handleLimparListaEventos(MethodChannel.Result result) {
        try {
            ACBrNFe.LimparListaEventos();
            result.success("Lista de eventos limpa com sucesso");
        } catch (Exception e) {
            result.error("Erro ao limpar lista de eventos", e.getMessage(), e);
        }
    }

    private void handleAssinar(MethodChannel.Result result){
        try {
            ACBrNFe.Assinar();
            result.success("Assinado com sucesso");
        } catch (Exception e) {
            result.error("Erro ao assinar", e.getMessage(), e);
        }
    }

    private void handleValidar(MethodChannel.Result result){
        try {
            ACBrNFe.Validar();
            result.success("Validado com sucesso");
        } catch (Exception e) {
            result.error("Erro ao validar", e.getMessage(), e);
        }
    }

    private void handleValidarRegrasdeNegocios(MethodChannel.Result result){
        try{
            String retorno = ACBrNFe.ValidarRegrasdeNegocios();
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao validar regras de negócio", e.getMessage(), e);
        }
    }

    private void handleVerificarAssinatura(MethodChannel.Result result){
        try{
            String retorno = ACBrNFe.VerificarAssinatura();
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao validar regras de negócio", e.getMessage(), e);
        }
    }

    private void handleGerarChave(MethodCall call, MethodChannel.Result result){
        Integer ACodigoUFArg = call.argument("ACodigoUF");
        if (ACodigoUFArg == null) {
            result.error("Argumento inválido", "ACodigoUF não pode ser nulo", null);
            return;
        }
        int ACodigoUF = ACodigoUFArg;

        Integer ACodigoNumericoArg = call.argument("ACodigoNumerico");
        if (ACodigoNumericoArg == null) {
            result.error("Argumento inválido", "ACodigoNumerico não pode ser nulo", null);
            return;
        }
        int ACodigoNumerico = ACodigoNumericoArg;

        Integer AModeloArg = call.argument("AModelo");
        if (AModeloArg == null) {
            result.error("Argumento inválido", "AModelo não pode ser nulo", null);
            return;
        }
        int AModelo = AModeloArg;

        Integer ASerieArg = call.argument("ASerie");
        if (ASerieArg == null) {
            result.error("Argumento inválido", "ASerie não pode ser nulo", null);
            return;
        }
        int ASerie = ASerieArg;

        Integer ANumeroArg = call.argument("ANumero");
        if (ANumeroArg == null) {
            result.error("Argumento inválido", "ANumero não pode ser nulo", null);
            return;
        }
        int ANumero = ANumeroArg;

        Integer ATpEmiArg = call.argument("ATpEmi");
        if (ATpEmiArg == null) {
            result.error("Argumento inválido", "ATpEmi não pode ser nulo", null);
            return;
        }
        int ATpEmi = ATpEmiArg;

        String AEmissao = call.argument("AEmissao");
        String ACNPJCPF = call.argument("ACNPJCPF");

        try{
            String retorno = ACBrNFe.GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi, AEmissao, ACNPJCPF);
            result.success(retorno);
        } catch (Exception e){
            result.error("Erro ao gerar chave", e.getMessage(), e);
        }
    }

    private void handleObterCertificados(MethodChannel.Result result){
        try{
            String retorno = ACBrNFe.ObterCertificados();
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao obter certificado", e.getMessage(), e);
        }
    }

    private void handleGetPath(MethodCall call, MethodChannel.Result result){
        Integer ATipoArg = call.argument("ATipo");
        if (ATipoArg == null) {
            result.error("Argumento inválido", "ATipo não pode ser nulo", null);
            return;
        }
        int ATipo = ATipoArg;
        try{
            String retorno = ACBrNFe.GetPath(ATipo);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao obter caminho", e.getMessage(), e);
        }
    }

    private void handleGetPathEvento(MethodCall call, MethodChannel.Result result){
        String ACodEvento = call.argument("ACodEvento");
        try{
            String retorno = ACBrNFe.GetPathEvento(ACodEvento);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao obter caminho do Evento", e.getMessage(), e);
        }
    }

    private void handleStatusServico(MethodChannel.Result result){
        try{
            String retorno = ACBrNFe.StatusServico();
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao obter Status Serviço", e.getMessage(), e);
        }
    }

    private void handleConsultar(MethodCall call, MethodChannel.Result result){
        String eChaveOuNFe = call.argument("eChaveOuNFe");
        boolean AExtrairEventos = false;
        try{
            String retorno = ACBrNFe.Consultar(eChaveOuNFe, AExtrairEventos);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao Consultar", e.getMessage(), e);
        }
    }

    private void handleConsultarRecibo(MethodCall call, MethodChannel.Result result){
        String ARecibo = call.argument("ARecibo");
        try{
            String retorno = ACBrNFe.ConsultarRecibo(ARecibo);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao Consultar Recibo", e.getMessage(), e);
        }
    }

    private void handleConsultaCadastro(MethodCall call, MethodChannel.Result result){
        String cUF = call.argument("cUF");
        String nDocumento = call.argument("nDocumento");
        boolean nIE = false;
        try{
            String retorno = ACBrNFe.ConsultaCadastro(cUF, nDocumento, nIE);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao Consultar Cadastro", e.getMessage(), e);
        }
    }

    private void handleInutilizar(MethodCall call, MethodChannel.Result result){
        String ACNPJ = call.argument("ACNPJ");
        String AJustificativa = call.argument("AJustificativa");
        Integer anoArgs = call.argument("Ano");

        if (anoArgs == null) {
            result.error("Argumento inválido", "Ano não pode ser nulo", null);
            return;
        }
        int Ano = anoArgs;

        Integer modeloArgs = call.argument("Modelo");
        if (modeloArgs == null){
            result.error("Argumento Inválido", "Modelo não pode ser nulo", null);
            return;
        }
        int Modelo = modeloArgs;

        Integer serieArgs = call.argument("Serie");
        if (serieArgs == null){
            result.error("Argumento Inválido", "Série não pode ser nulo", null);
            return;
        }
        int Serie = serieArgs;

        Integer numeroInicialArgs = call.argument("Serie");
        if (numeroInicialArgs == null){
            result.error("Argumento Inválido", "Numero Inicial não pode ser nulo", null);
            return;
        }
        int numeroInicial = numeroInicialArgs;

        Integer numeroFinalArgs = call.argument("Serie");
        if (numeroFinalArgs == null){
            result.error("Argumento Inválido", "Numero Final não pode ser nulo", null);
            return;
        }
        int numeroFinal = numeroFinalArgs;

        try{
            String retorno = ACBrNFe.Inutilizar(ACNPJ, AJustificativa, Ano, Modelo, Serie, numeroInicial, numeroFinal);
            result.success(retorno);
        } catch (Exception e){
            result.error("Erro ao Inutilizar", e.getMessage(), e);
        }
    }

    private void handleEnviar(MethodCall call, MethodChannel.Result result){
        Integer loteArgs = call.argument("ALote");
        if (loteArgs == null) {
            result.error("Argumento inválido", "ALote não pode ser nulo", null);
            return;
        }
        int ALote = loteArgs;

        boolean AImprimir = false;
        boolean ASincrono = false;
        boolean AZipado = false;

        try {
            String retorno = ACBrNFe.Enviar(ALote, AImprimir, ASincrono, AZipado);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao Enviar", e.getMessage(), e);
        }
    }

    private void handleCancelar(MethodCall call, MethodChannel.Result result){

        String eChave = call.argument("eChave");
        String AJustificativa = call.argument("AJustificativa");
        String eCNPJ = call.argument("eCNPJ");

        Integer loteArgs = call.argument("ALote");
        if (loteArgs == null) {
            result.error("Argumento inválido", "ALote não pode ser nulo", null);
            return;
        }
        int ALote = loteArgs;

        try {
            String retorno = ACBrNFe.Cancelar(eChave, AJustificativa, eCNPJ, ALote);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao Cancelar", e.getMessage(), e);
        }
    }

    private void handleEnviarEvento(MethodCall call, MethodChannel.Result result){
        Integer idLoteArgs = call.argument("idLote");
        if (idLoteArgs == null) {
            result.error("Argumento inválido", "idLote não pode ser nulo", null);
            return;
        }
        int idLote = idLoteArgs;

        try {
            String retorno = ACBrNFe.EnviarEvento(idLote);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao Enviar Evento", e.getMessage(), e);
        }
    }

    private void handleDistribuicaoDFe(MethodCall call, MethodChannel.Result result){
        Integer cUFAutorArgs = call.argument("AcUFAutor");
        if (cUFAutorArgs == null) {
            result.error("Argumento inválido", "AcUFAutor não pode ser nulo", null);
            return;
        }
        int AcUFAutor = cUFAutorArgs;

        String eCNPJCPF = call.argument("eCNPJCPF");
        String eUltNSU = call.argument("eUltNSU");
        String eArquivoOuXML = call.argument("eArquivoOuXML");

        try {
            String retorno = ACBrNFe.DistribuicaoDFe(AcUFAutor, eCNPJCPF, eUltNSU, eArquivoOuXML);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao Distribuir DFe", e.getMessage(), e);
        }
    }

    private void handleDistribuicaoDFePorUltNSU(MethodCall call, MethodChannel.Result result){
        Integer cUFAutorArgs = call.argument("AcUFAutor");
        if (cUFAutorArgs == null) {
            result.error("Argumento inválido", "AcUFAutor não pode ser nulo", null);
            return;
        }
        int AcUFAutor = cUFAutorArgs;

        String eCNPJCPF = call.argument("eCNPJCPF");
        String eUltNSU = call.argument("eUltNSU");

        try {
            String retorno = ACBrNFe.DistribuicaoDFePorUltNSU(AcUFAutor, eCNPJCPF, eUltNSU);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao Distribuir DFe por Ult NSU", e.getMessage(), e);
        }
    }

    private void handleDistribuicaoDFePorNSU(MethodCall call, MethodChannel.Result result){
        Integer cUFAutorArgs = call.argument("AcUFAutor");
        if (cUFAutorArgs == null) {
            result.error("Argumento inválido", "AcUFAutor não pode ser nulo", null);
            return;
        }
        int AcUFAutor = cUFAutorArgs;

        String eCNPJCPF = call.argument("eCNPJCPF");
        String eNSU = call.argument("eNSU");

        try {
            String retorno = ACBrNFe.DistribuicaoDFePorNSU(AcUFAutor, eCNPJCPF, eNSU);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao Distribuir DFe por NSU", e.getMessage(), e);
        }
    }

    private void handleDistribuicaoDFePorChave(MethodCall call, MethodChannel.Result result){
        Integer cUFAutorArgs = call.argument("AcUFAutor");
        if (cUFAutorArgs == null) {
            result.error("Argumento inválido", "AcUFAutor não pode ser nulo", null);
            return;
        }
        int AcUFAutor = cUFAutorArgs;

        String eCNPJCPF = call.argument("eCNPJCPF");
        String eChave = call.argument("eChave");

        try {
            String retorno = ACBrNFe.DistribuicaoDFePorChave(AcUFAutor, eCNPJCPF, eChave);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao Distribuir DFe por Chave", e.getMessage(), e);
        }
    }

    private void handleEnviarEmail(MethodCall call, MethodChannel.Result result){
        String ePara = call.argument("ePara");
        String eXMLNFe = call.argument("eXMLNFe");
        boolean AEnviaPDF = false;
        String eAssunto = call.argument("eAssunto");
        String eCC = call.argument("eCC");
        String eAnexos = call.argument("eAnexos");
        String eMensagem = call.argument("eMensagem");

        try {
            ACBrNFe.EnviarEmail(ePara, eXMLNFe, AEnviaPDF, eAssunto, eCC, eAnexos, eMensagem);
            result.success("Email enviado com sucesso");
        } catch (Exception e) {
            result.error("Erro ao Enviar Email", e.getMessage(), e);
        }
    }

    private void handleEnviarEmailEvento(MethodCall call, MethodChannel.Result result){
        String ePara = call.argument("ePara");
        String eChaveEvento = call.argument("eChaveEvento");
        String eChaveNFe = call.argument("eChaveNFe");
        boolean AEnviaPDF = false;
        String eAssunto = call.argument("eAssunto");
        String eCC = call.argument("eCC");
        String eAnexos = call.argument("eAnexos");
        String eMensagem = call.argument("eMensagem");

        try {
            ACBrNFe.EnviarEmailEvento(ePara, eChaveEvento, eChaveNFe, AEnviaPDF, eAssunto, eCC, eAnexos, eMensagem);
            result.success("Email Evento enviado com sucesso");
        } catch (Exception e) {
            result.error("Erro ao Enviar Email Evento", e.getMessage(), e);
        }
    }

    private void handleImprimir(MethodCall call, MethodChannel.Result result){
        String cImpressora = call.argument("cImpressora");

        Integer nNumCopiasArg = call.argument("nNumCopias");
        if (nNumCopiasArg == null) {
            result.error("Argumento inválido", "nNumCopias não pode ser nulo", null);
            return;
        }
        int nNumCopias = nNumCopiasArg;

        String cProtocolo = call.argument("cProtocolo");
        String bMostrarPreview = call.argument("bMostrarPreview");
        String cMarcaDagua = call.argument("cMarcaDagua");
        String bViaConsumidor = call.argument("bViaConsumidor");
        String bSimplificado = call.argument("bSimplificado");

        try {
            ACBrNFe.Imprimir(cImpressora, nNumCopias, cProtocolo, bMostrarPreview, cMarcaDagua, bViaConsumidor, bSimplificado);
            result.success("Impressão realizada com sucesso");
        } catch (Exception e) {
            result.error("Erro ao Imprimir", e.getMessage(), e);
        }
    }

    private void handleImprimirPDF(MethodCall call, MethodChannel.Result result){
        try {
            ACBrNFe.ImprimirPDF();
            result.success("Impressão PDF realizada com sucesso");
        } catch (Exception e) {
            result.error("Erro ao Imprimir PDF", e.getMessage(), e);
        }
    }

    private void handleSalvarPDF(MethodCall call, MethodChannel.Result result){
        try {
            String retorno = ACBrNFe.SalvarPDF();
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao Imprimir PDF", e.getMessage(), e);
        }
    }

    private void handleImprimirEvento(MethodCall call, MethodChannel.Result result){

        String eArquivoXmlNFe = call.argument("eArquivoXmlNFe");
        String eArquivoXmlEvento = call.argument("eArquivoXmlEvento");

        try {
            ACBrNFe.ImprimirEvento(eArquivoXmlNFe, eArquivoXmlEvento);
            result.success("Impressão Evento realizada com sucesso");
        } catch (Exception e) {
            result.error("Erro ao Imprimir Evento", e.getMessage(), e);
        }
    }

    private void handleImprimirEventoPDF(MethodCall call, MethodChannel.Result result){

        String eArquivoXmlNFe = call.argument("eArquivoXmlNFe");
        String eArquivoXmlEvento = call.argument("eArquivoXmlEvento");

        try {
            ACBrNFe.ImprimirEventoPDF(eArquivoXmlNFe, eArquivoXmlEvento);
            result.success("Impressão Evento PDF realizada com sucesso");
        } catch (Exception e) {
            result.error("Erro ao Imprimir Evento PDF", e.getMessage(), e);
        }
    }

    private void handleSalvarEventoPDF(MethodCall call, MethodChannel.Result result){

        String eArquivoXmlNFe = call.argument("eArquivoXmlNFe");
        String eArquivoXmlEvento = call.argument("eArquivoXmlEvento");

        try {
            String retorno = ACBrNFe.SalvarEventoPDF(eArquivoXmlNFe, eArquivoXmlEvento);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao Imprimir Evento PDF", e.getMessage(), e);
        }
    }

    private void handleImprimirInutilizacao(MethodCall call, MethodChannel.Result result){

        String eArquivoXml = call.argument("eArquivoXml");

        try {
            ACBrNFe.ImprimirInutilizacao(eArquivoXml);
            result.success("Impressão Inutilização realizada com sucesso");
        } catch (Exception e) {
            result.error("Erro ao Imprimir Inutilização", e.getMessage(), e);
        }
    }

    private void handleImprimirInutilizacaoPDF(MethodCall call, MethodChannel.Result result){
        String eArquivoXml = call.argument("eArquivoXml");

        try {
            ACBrNFe.ImprimirInutilizacaoPDF(eArquivoXml);
            result.success("Impressão Inutilização realizada com sucesso");
        } catch (Exception e) {
            result.error("Erro ao Imprimir Inutilização", e.getMessage(), e);
        }
    }

    private void handleSalvarInutilizacaoPDF(MethodCall call, MethodChannel.Result result){
        String eArquivoXml = call.argument("eArquivoXml");

        try {
            String retorno = ACBrNFe.SalvarInutilizacaoPDF(eArquivoXml);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro ao Imprimir Inutilização", e.getMessage(), e);
        }
    }

    private void aplicarConfiguracoesPadrao() throws Exception{
        ACBrNFe.configGravarValor("Principal", "LogPath", appDir.getAbsolutePath());
        ACBrNFe.configGravarValor("Principal", "LogNivel", "4");
        ACBrNFe.configGravar();
    }
}
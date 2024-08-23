<?php
/* {******************************************************************************}
// { Projeto: Componentes ACBr                                                    }
// {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
// { mentos de Automação Comercial utilizados no Brasil                           }
// {                                                                              }
// { Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
// {                                                                              }
// { Colaboradores nesse arquivo: Renato Rubinho                                  }
// {                                                                              }
// {  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
// { Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
// {                                                                              }
// {  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
// { sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
// { Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
// { qualquer versão posterior.                                                   }
// {                                                                              }
// {  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
// { NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
// { ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
// { do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
// {                                                                              }
// {  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
// { com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
// { no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
// { Você também pode obter uma copia da licença em:                              }
// { http://www.opensource.org/licenses/lgpl-license.php                          }
// {                                                                              }
// { Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
// {       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
// {******************************************************************************}
*/
header('Content-Type: application/json; charset=UTF-8');

include 'ACBrNFeST.php';
include '../../ACBrComum/ACBrComum.php';

$nomeLib = "ACBrNFe";
$metodo = $_POST['metodo'];

if (ValidaFFI() != 0)
    exit;

$dllPath = CarregaDll(__DIR__, $nomeLib);

if ($dllPath == -10)
    exit;

$importsPath = CarregaImports(__DIR__, $nomeLib, 'ST');

if ($importsPath == -10)
    exit;

$iniPath = CarregaIniPath(__DIR__, $nomeLib);

$processo = "file_get_contents";
$ffi = CarregaContents($importsPath, $dllPath);

try {
    $resultado = "";
    $processo = "Inicializar";

    $processo = "NFE_Inicializar";
    if (Inicializar($ffi, $iniPath) != 0)
        exit;

    if ($metodo == "salvarConfiguracoes") {
        $processo = $metodo . "/" . "NFE_ConfigGravarValor";

        if (ConfigGravarValor($ffi, "NFe", "AtualizarXMLCancelado", $_POST['atualizarXml']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "ExibirErroSchema", $_POST['exibirErroSchema']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "FormatoAlerta", $_POST['formatoAlerta']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "FormaEmissao", $_POST['formaEmissao']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "ModeloDF", $_POST['modeloDF']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "VersaoDF", $_POST['versaoDF']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "RetirarAcentos", $_POST['retirarAcentos']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "SalvarGer", $_POST['SalvarGer']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "PathSalvar", $_POST['pathSalvar']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "PathSchemas", $_POST['pathSchemas']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "IdCSRT", $_POST['idCSRT']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "CSRT", $_POST['CSRT']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "SSLType", $_POST['SSLType']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "Timeout", $_POST['timeout']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "Ambiente", $_POST['ambiente']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "Visualizar", $_POST['visualizar']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "SalvarWS", $_POST['SalvarWS']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "AjustaAguardaConsultaRet", $_POST['ajustaAguardaConsultaRet']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "AguardarConsultaRet", $_POST['aguardarConsultaRet']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "Tentativas", $_POST['tentativas']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "IntervaloTentativas", $_POST['intervaloTentativas']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "SalvarArq", $_POST['SalvarArq']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "SepararPorMes", $_POST['SepararPorMes']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "AdicionarLiteral", $_POST['AdicionarLiteral']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "EmissaoPathNFe", $_POST['EmissaoPathNFe']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "SalvarEvento", $_POST['SalvarEvento']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "SepararPorCNPJ", $_POST['SepararPorCNPJ']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "SepararPorModelo", $_POST['SepararPorModelo']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "PathNFe", $_POST['PathNFe']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "PathInu", $_POST['PathInu']) != 0) exit;
        if (ConfigGravarValor($ffi, "NFe", "PathEvento", $_POST['PathEvento']) != 0) exit;

        if (ConfigGravarValor($ffi, "Proxy", "Servidor", $_POST['proxyServidor']) != 0) exit;
        if (ConfigGravarValor($ffi, "Proxy", "Porta", $_POST['proxyPorta']) != 0) exit;
        if (ConfigGravarValor($ffi, "Proxy", "Usuario", $_POST['proxyUsuario']) != 0) exit;
        if (ConfigGravarValor($ffi, "Proxy", "Senha", $_POST['proxySenha']) != 0) exit;

        if (ConfigGravarValor($ffi, "DFe", "UF", $_POST['UF']) != 0) exit;
        if (ConfigGravarValor($ffi, "DFe", "SSLCryptLib", $_POST['SSLCryptLib']) != 0) exit;
        if (ConfigGravarValor($ffi, "DFe", "SSLHttpLib", $_POST['SSLHttpLib']) != 0) exit;
        if (ConfigGravarValor($ffi, "DFe", "SSLXmlSignLib", $_POST['SSLXmlSignLib']) != 0) exit;
        if (ConfigGravarValor($ffi, "DFe", "ArquivoPFX", $_POST['ArquivoPFX']) != 0) exit;
        if (ConfigGravarValor($ffi, "DFe", "DadosPFX", $_POST['DadosPFX']) != 0) exit;
        if (ConfigGravarValor($ffi, "DFe", "Senha", $_POST['senhaCertificado']) != 0) exit;
        if (ConfigGravarValor($ffi, "DFe", "NumeroSerie", $_POST['NumeroSerie']) != 0) exit;

        if (ConfigGravarValor($ffi, "DANFE", "PathLogo", $_POST['PathLogo']) != 0) exit;
        if (ConfigGravarValor($ffi, "DANFE", "TipoDANFE", $_POST['TipoDANFE']) != 0) exit;
        // Mesmo caminho da pasta para salvar aquivos da NFe
        if (ConfigGravarValor($ffi, "DANFE", "PathPDF", $_POST['PathNFe']) != 0) exit;

        if (ConfigGravarValor($ffi, "DANFENFCe", "TipoRelatorioBobina", $_POST['TipoRelatorioBobina']) != 0) exit;

        if (ConfigGravarValor($ffi, "PosPrinter", "Modelo", $_POST['PosPrinterModelo']) != 0) exit;
        if (ConfigGravarValor($ffi, "PosPrinter", "PaginaDeCodigo", $_POST['PaginaDeCodigo']) != 0) exit;
        if (ConfigGravarValor($ffi, "PosPrinter", "Porta", $_POST['PosPrinterPorta']) != 0) exit;
        if (ConfigGravarValor($ffi, "PosPrinter", "ColunasFonteNormal", $_POST['ColunasFonteNormal']) != 0) exit;
        if (ConfigGravarValor($ffi, "PosPrinter", "EspacoEntreLinhas", $_POST['EspacoEntreLinhas']) != 0) exit;
        if (ConfigGravarValor($ffi, "PosPrinter", "LinhasBuffer", $_POST['LinhasBuffer']) != 0) exit;
        if (ConfigGravarValor($ffi, "PosPrinter", "LinhasEntreCupons", $_POST['LinhasEntreCupons']) != 0) exit;
        if (ConfigGravarValor($ffi, "PosPrinter", "ControlePorta", $_POST['ControlePorta']) != 0) exit;
        if (ConfigGravarValor($ffi, "PosPrinter", "TraduzirTags", $_POST['TraduzirTags']) != 0) exit;
        if (ConfigGravarValor($ffi, "PosPrinter", "CortaPapel", $_POST['CortaPapel']) != 0) exit;
        if (ConfigGravarValor($ffi, "PosPrinter", "IgnorarTags", $_POST['IgnorarTags']) != 0) exit;

        if (ConfigGravarValor($ffi, "Email", "Nome", $_POST['emailNome']) != 0) exit;
        if (ConfigGravarValor($ffi, "Email", "Conta", $_POST['emailConta']) != 0) exit;
        if (ConfigGravarValor($ffi, "Email", "Servidor", $_POST['emailServidor']) != 0) exit;
        if (ConfigGravarValor($ffi, "Email", "Porta", $_POST['emailPorta']) != 0) exit;
        if (ConfigGravarValor($ffi, "Email", "SSL", $_POST['emailSSL']) != 0) exit;
        if (ConfigGravarValor($ffi, "Email", "TLS", $_POST['emailTLS']) != 0) exit;
        if (ConfigGravarValor($ffi, "Email", "Usuario", $_POST['emailUsuario']) != 0) exit;
        if (ConfigGravarValor($ffi, "Email", "Senha", $_POST['emailSenha']) != 0) exit;

        $resultado = "Configurações salvas com sucesso.";
    }

    if ($metodo == "carregarConfiguracoes") {
        $processo = $metodo . "/" . "NFE_ConfigLer";

        if (ConfigLerValor($ffi, "NFe", "AtualizarXMLCancelado", $atualizarXml) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "ExibirErroSchema", $exibirErroSchema) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "FormatoAlerta", $formatoAlerta) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "FormaEmissao", $formaEmissao) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "ModeloDF", $modeloDF) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "VersaoDF", $versaoDF) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "RetirarAcentos", $retirarAcentos) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "SalvarGer", $SalvarGer) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "PathSalvar", $pathSalvar) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "PathSchemas", $pathSchemas) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "IdCSRT", $idCSRT) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "CSRT", $CSRT) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "SSLType", $SSLType) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "Timeout", $timeout) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "Ambiente", $ambiente) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "Visualizar", $visualizar) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "SalvarWS", $SalvarWS) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "AjustaAguardaConsultaRet", $ajustaAguardaConsultaRet) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "AguardarConsultaRet", $aguardarConsultaRet) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "Tentativas", $tentativas) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "IntervaloTentativas", $intervaloTentativas) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "SalvarArq", $SalvarArq) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "SepararPorMes", $SepararPorMes) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "AdicionarLiteral", $AdicionarLiteral) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "EmissaoPathNFe", $EmissaoPathNFe) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "SalvarEvento", $SalvarEvento) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "SepararPorCNPJ", $SepararPorCNPJ) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "SepararPorModelo", $SepararPorModelo) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "PathNFe", $PathNFe) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "PathInu", $PathInu) != 0) exit;
        if (ConfigLerValor($ffi, "NFe", "PathEvento", $PathEvento) != 0) exit;

        if (ConfigLerValor($ffi, "Proxy", "Servidor", $proxyServidor) != 0) exit;
        if (ConfigLerValor($ffi, "Proxy", "Porta", $proxyPorta) != 0) exit;
        if (ConfigLerValor($ffi, "Proxy", "Usuario", $proxyUsuario) != 0) exit;
        if (ConfigLerValor($ffi, "Proxy", "Senha", $proxySenha) != 0) exit;

        if (ConfigLerValor($ffi, "DFe", "UF", $UF) != 0) exit;
        if (ConfigLerValor($ffi, "DFe", "SSLCryptLib", $SSLCryptLib) != 0) exit;
        if (ConfigLerValor($ffi, "DFe", "SSLHttpLib", $SSLHttpLib) != 0) exit;
        if (ConfigLerValor($ffi, "DFe", "SSLXmlSignLib", $SSLXmlSignLib) != 0) exit;
        if (ConfigLerValor($ffi, "DFe", "ArquivoPFX", $ArquivoPFX) != 0) exit;
        if (ConfigLerValor($ffi, "DFe", "DadosPFX", $DadosPFX) != 0) exit;
        if (ConfigLerValor($ffi, "DFe", "Senha", $senhaCertificado) != 0) exit;
        if (ConfigLerValor($ffi, "DFe", "NumeroSerie", $NumeroSerie) != 0) exit;

        if (ConfigLerValor($ffi, "DANFE", "PathLogo", $PathLogo) != 0) exit;
        if (ConfigLerValor($ffi, "DANFE", "TipoDANFE", $TipoDANFE) != 0) exit;

        if (ConfigLerValor($ffi, "DANFENFCe", "TipoRelatorioBobina", $TipoRelatorioBobina) != 0) exit;

        if (ConfigLerValor($ffi, "PosPrinter", "Modelo", $PosPrinterModelo) != 0) exit;
        if (ConfigLerValor($ffi, "PosPrinter", "PaginaDeCodigo", $PaginaDeCodigo) != 0) exit;
        if (ConfigLerValor($ffi, "PosPrinter", "Porta", $PosPrinterPorta) != 0) exit;
        if (ConfigLerValor($ffi, "PosPrinter", "ColunasFonteNormal", $ColunasFonteNormal) != 0) exit;
        if (ConfigLerValor($ffi, "PosPrinter", "EspacoEntreLinhas", $EspacoEntreLinhas) != 0) exit;
        if (ConfigLerValor($ffi, "PosPrinter", "LinhasBuffer", $LinhasBuffer) != 0) exit;
        if (ConfigLerValor($ffi, "PosPrinter", "LinhasEntreCupons", $LinhasEntreCupons) != 0) exit;
        if (ConfigLerValor($ffi, "PosPrinter", "ControlePorta", $ControlePorta) != 0) exit;
        if (ConfigLerValor($ffi, "PosPrinter", "TraduzirTags", $TraduzirTags) != 0) exit;
        if (ConfigLerValor($ffi, "PosPrinter", "CortaPapel", $CortaPapel) != 0) exit;
        if (ConfigLerValor($ffi, "PosPrinter", "IgnorarTags", $IgnorarTags) != 0) exit;

        if (ConfigLerValor($ffi, "Email", "Nome", $emailNome) != 0) exit;
        if (ConfigLerValor($ffi, "Email", "Conta", $emailConta) != 0) exit;
        if (ConfigLerValor($ffi, "Email", "Servidor", $emailServidor) != 0) exit;
        if (ConfigLerValor($ffi, "Email", "Porta", $emailPorta) != 0) exit;
        if (ConfigLerValor($ffi, "Email", "SSL", $emailSSL) != 0) exit;
        if (ConfigLerValor($ffi, "Email", "TLS", $emailTLS) != 0) exit;
        if (ConfigLerValor($ffi, "Email", "Usuario", $emailUsuario) != 0) exit;
        if (ConfigLerValor($ffi, "Email", "Senha", $emailSenha) != 0) exit;

        $processo = $metodo . "/" . "responseData";
        $responseData = [
            'dados' => [
                'atualizarXml' => $atualizarXml ?? '',
                'exibirErroSchema' => $exibirErroSchema ?? '',
                'formatoAlerta' => $formatoAlerta ?? '',
                'formaEmissao' => $formaEmissao ?? '',
                'modeloDF' => $modeloDF ?? '',
                'versaoDF' => $versaoDF ?? '',
                'retirarAcentos' => $retirarAcentos ?? '',
                'SalvarGer' => $SalvarGer ?? '',
                'pathSalvar' => $pathSalvar ?? '',
                'pathSchemas' => $pathSchemas ?? '',
                'idCSRT' => $idCSRT ?? '',
                'CSRT' => $CSRT ?? '',
                'SSLType' => $SSLType ?? '',
                'timeout' => $timeout ?? '',
                'ambiente' => $ambiente ?? '',
                'visualizar' => $visualizar ?? '',
                'SalvarWS' => $SalvarWS ?? '',
                'ajustaAguardaConsultaRet' => $ajustaAguardaConsultaRet ?? '',
                'aguardarConsultaRet' => $aguardarConsultaRet ?? '',
                'tentativas' => $tentativas ?? '',
                'intervaloTentativas' => $intervaloTentativas ?? '',
                'SalvarArq' => $SalvarArq ?? '',
                'SepararPorMes' => $SepararPorMes ?? '',
                'AdicionarLiteral' => $AdicionarLiteral ?? '',
                'EmissaoPathNFe' => $EmissaoPathNFe ?? '',
                'SalvarEvento' => $SalvarEvento ?? '',
                'SepararPorCNPJ' => $SepararPorCNPJ ?? '',
                'SepararPorModelo' => $SepararPorModelo ?? '',
                'PathNFe' => $PathNFe ?? '',
                'PathInu' => $PathInu ?? '',
                'PathEvento' => $PathEvento ?? '',

                'proxyServidor' => $proxyServidor ?? '',
                'proxyPorta' => $proxyPorta ?? '',
                'proxyUsuario' => $proxyUsuario ?? '',
                'proxySenha' => $proxySenha ?? '',

                'UF' => $UF ?? '',
                'SSLCryptLib' => $SSLCryptLib ?? '',
                'SSLHttpLib' => $SSLHttpLib ?? '',
                'SSLXmlSignLib' => $SSLXmlSignLib ?? '',
                'ArquivoPFX' => $ArquivoPFX ?? '',
                'DadosPFX' => $DadosPFX ?? '',
                'senhaCertificado' => $senhaCertificado ?? '',
                'NumeroSerie' => $NumeroSerie ?? '',

                'PathLogo' => $PathLogo ?? '',
                'TipoDANFE' => $TipoDANFE ?? '',

                'TipoRelatorioBobina' => $TipoRelatorioBobina ?? '',

                'PosPrinterModelo' => $PosPrinterModelo ?? '',
                'PaginaDeCodigo' => $PaginaDeCodigo ?? '',
                'PosPrinterPorta' => $PosPrinterPorta ?? '',
                'ColunasFonteNormal' => $ColunasFonteNormal ?? '',
                'EspacoEntreLinhas' => $EspacoEntreLinhas ?? '',
                'LinhasBuffer' => $LinhasBuffer ?? '',
                'LinhasEntreCupons' => $LinhasEntreCupons ?? '',
                'ControlePorta' => $ControlePorta ?? '',
                'TraduzirTags' => $TraduzirTags ?? '',
                'CortaPapel' => $CortaPapel ?? '',
                'IgnorarTags' => $IgnorarTags ?? '',

                'emailNome' => $emailNome ?? '',
                'emailConta' => $emailConta ?? '',
                'emailServidor' => $emailServidor ?? '',
                'emailPorta' => $emailPorta ?? '',
                'emailSSL' => $emailSSL ?? '',
                'emailTLS' => $emailTLS ?? '',
                'emailUsuario' => $emailUsuario ?? '',
                'emailSenha' => $emailSenha ?? ''
            ]
        ];
    }

    if ($metodo == "statusServico") {
        $processo = "NFE_StatusServico";

        if (StatusServico($ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "OpenSSLInfo") {
        $processo = "NFE_OpenSSLInfo";

        if (OpenSSLInfo($ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "CarregarXmlNfe") {
        $processo = "NFE_CarregarXml";

        if (CarregarXmlNfe($ffi, $_POST['conteudoArquivo01'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "CarregarIniNfe") {
        $processo = "NFE_CarregarINI";

        if (CarregarINI($ffi, $_POST['conteudoArquivo01'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "CarregarEventoXML") {
        $processo = "NFE_CarregarEventoXml";

        if (CarregarEventoXML($ffi, $_POST['conteudoArquivo01'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "LimparListaNfe") {
        $processo = "NFE_LimparListaNfe";

        if (LimparListaNfe($ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "LimparListaEventos") {
        $processo = "NFE_LimparListaEventos";

        if (LimparListaEventos($ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "AssinarNFe") {
        $processo = "NFE_AssinarNFe";

        if (AssinarNFe($ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ValidarNFe") {
        $processo = "NFE_ValidarNFe";

        if (ValidarNFe($ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ValidarRegrasdeNegocios") {
        $processo = "NFE_CarregarXml";

        if (CarregarXmlNfe($ffi, $_POST['AeArquivoXmlNFe'], $resultado) != 0) {
            exit;
        }

        $processo = "NFE_ValidarRegrasdeNegocios";

        if (ValidarRegrasdeNegocios($ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "GerarChave") {
        $processo = "NFE_GerarChave";

        if (GerarChave(
            $ffi,
            $_POST['ACodigoUF'],
            $_POST['ACodigoNumerico'],
            $_POST['AModelo'],
            $_POST['ASerie'],
            $_POST['ANumero'],
            $_POST['ATpEmi'],
            $_POST['AEmissao'],
            $_POST['ACNPJCPF'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "Consultar") {
        $processo = "NFE_Consultar";

        if (Consultar($ffi, $_POST['eChaveOuNFe'], $_POST['AExtrairEventos'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "Inutilizar") {
        $processo = "NFE_Inutilizar";

        if (Inutilizar(
            $ffi,
            $_POST['ACNPJ'],
            $_POST['AJustificativa'],
            $_POST['AAno'],
            $_POST['AModelo'],
            $_POST['ASerie'],
            $_POST['ANumeroInicial'],
            $_POST['ANumeroFinal'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "Enviar") {
        if ($_POST['tipoArquivo'] == "xml") {
            $processo = "NFE_CarregarXml";

            if (CarregarXmlNfe($ffi, $_POST['AeArquivoNFe'], $resultado) != 0) {
                exit;
            }
        } else {
            $processo = "NFE_CarregarINI";

            if (CarregarINI($ffi, $_POST['AeArquivoNFe'], $resultado) != 0) {
                exit;
            }
        }

        $processo = "NFE_AssinarNFe";

        if (AssinarNFe($ffi, $resultado) != 0) {
            exit;
        }

        $processo = "NFE_Enviar";

        if (Enviar(
            $ffi,
            $_POST['ALote'],
            $_POST['AImprimir'],
            $_POST['ASincrono'],
            $_POST['AZipado'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarRecibo") {
        $processo = "NFE_ConsultarRecibo";

        if (ConsultarRecibo($ffi, $_POST['ARecibo'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "Cancelar") {
        $processo = "NFE_Cancelar";

        if (Cancelar($ffi, $_POST['AeChave'], $_POST['AeJustificativa'], $_POST['AeCNPJCPF'], $_POST['ALote'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "EnviarEvento") {
        $processo = "NFE_CarregarXml";

        if (CarregarXmlNfe($ffi, $_POST['AeArquivoXmlNFe'], $resultado) != 0) {
            exit;
        }

        $processo = "NFE_CarregarEventoXml";

        if (CarregarEventoXML($ffi, $_POST['AeArquivoXmlEvento'], $resultado) != 0) {
            exit;
        }

        $processo = "NFE_EnviarEvento";

        if (EnviarEvento($ffi, $_POST['AidLote'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultaCadastro") {
        $processo = "NFE_ConsultaCadastro";

        if (ConsultaCadastro($ffi, $_POST['AcUF'], $_POST['AnDocumento'], $_POST['AnIE'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "DistribuicaoDFePorUltNSU") {
        $processo = "NFE_DistribuicaoDFePorUltNSU";

        if (DistribuicaoDFePorUltNSU($ffi, $_POST['AcUFAutor'], $_POST['AeCNPJCPF'], $_POST['AeultNSU'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "DistribuicaoDFePorNSU") {
        $processo = "NFE_DistribuicaoDFePorNSU";

        if (DistribuicaoDFePorNSU($ffi, $_POST['AcUFAutor'], $_POST['AeCNPJCPF'], $_POST['AeNSU'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "DistribuicaoDFePorChave") {
        $processo = "NFE_DistribuicaoDFePorChave";

        if (DistribuicaoDFePorChave($ffi, $_POST['AcUFAutor'], $_POST['AeCNPJCPF'], $_POST['AechNFe'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "EnviarEmail") {
        $processo = "NFE_CarregarXml";

        if (CarregarXmlNfe($ffi, $_POST['AeArquivoXmlNFe'], $resultado) != 0) {
            exit;
        }

        $processo = "NFE_EnviarEmail";

        if (EnviarEmail($ffi, $_POST['AePara'], $_POST['AeChaveNFe'], $_POST['AEnviaPDF'], $_POST['AeAssunto'], $_POST['AeCC'], $_POST['AeAnexos'], $_POST['AeMensagem'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "EnviarEmailEvento") {
        $processo = "NFE_EnviarEmailEvento";

        if (EnviarEmailEvento($ffi, $_POST['AePara'], $_POST['AeChaveEvento'], $_POST['AeChaveNFe'], $_POST['AEnviaPDF'], $_POST['AeAssunto'], $_POST['AeCC'], $_POST['AeAnexos'], $_POST['AeMensagem'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ImprimirPDF") {
        $processo = "NFE_CarregarXml";

        if (CarregarXmlNfe($ffi, $_POST['AeArquivoXmlNFe'], $resultado) != 0) {
            exit;
        }

        $processo = "NFE_ImprimirPDF";

        if (ImprimirPDF($ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "SalvarPDF") {
        $processo = "NFE_CarregarXml";

        if (CarregarXmlNfe($ffi, $_POST['AeArquivoXmlNFe'], $resultado) != 0) {
            exit;
        }

        $processo = "NFE_SalvarPDF";

        if (SalvarPDF($ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ImprimirEventoPDF") {
        $processo = "NFE_ImprimirEventoPDF";

        if (ImprimirEventoPDF($ffi, $_POST['AeArquivoXmlNFe'], $_POST['AeArquivoXmlEvento'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "SalvarEventoPDF") {
        $processo = "NFE_SalvarEventoPDF";

        if (SalvarEventoPDF($ffi, $_POST['AeArquivoXmlNFe'], $_POST['AeArquivoXmlEvento'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ImprimirInutilizacaoPDF") {
        $processo = "NFE_ImprimirInutilizacaoPDF";

        if (ImprimirInutilizacaoPDF($ffi, $_POST['AeArquivoXml'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "SalvarInutilizacaoPDF") {
        $processo = "NFE_SalvarInutilizacaoPDF";

        if (SalvarInutilizacaoPDF($ffi, $_POST['AeArquivoXml'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo != "carregarConfiguracoes") {
        $processo = "responseData";
        $responseData = [
            'mensagem' => $resultado
        ];
    }
} catch (Exception $e) {
    $erro = $e->getMessage();
    echo json_encode(["mensagem" => "Exceção[$processo]: $erro"]);
    exit;
}

try {
    if ($processo != "NFE_Inicializar") {
        $processo = "NFE_Finalizar";
        if (Finalizar($ffi) != 0)
            exit;
    }
} catch (Exception $e) {
    $erro = $e->getMessage();
    echo json_encode(["mensagem" => "Exceção[$processo]: $erro"]);
    exit;
}

echo json_encode($responseData);

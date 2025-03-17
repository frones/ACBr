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

include 'ACBrReinfST.php';
include '../../ACBrComum/ACBrComum.php';

$nomeLib = "ACBrReinf";
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

    $processo = "Reinf_Inicializar";
    if (Inicializar($ffi, $iniPath) != 0)
        exit;

    if ($metodo == "salvarConfiguracoes") {
        $processo = $metodo . "/" . "Reinf_ConfigGravarValor";

        if (ConfigGravarValor($ffi, "Principal", "LogPath", $_POST['LogPath']) != 0) exit;
        if (ConfigGravarValor($ffi, "Principal", "LogNivel", $_POST['LogNivel']) != 0) exit;

        if (ConfigGravarValor($ffi, "Reinf", "ExibirErroSchema", $_POST['exibirErroSchema']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "FormatoAlerta", $_POST['formatoAlerta']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "FormaEmissao", $_POST['formaEmissao']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "VersaoDF", $_POST['versaoDF']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "RetirarAcentos", $_POST['retirarAcentos']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "SalvarGer", $_POST['SalvarGer']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "PathSalvar", $_POST['pathSalvar']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "PathSchemas", $_POST['pathSchemas']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "SSLType", $_POST['SSLType']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "Timeout", $_POST['timeout']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "Ambiente", $_POST['ambiente']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "Visualizar", $_POST['visualizar']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "SalvarWS", $_POST['SalvarWS']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "AjustaAguardaConsultaRet", $_POST['ajustaAguardaConsultaRet']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "AguardarConsultaRet", $_POST['aguardarConsultaRet']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "Tentativas", $_POST['tentativas']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "IntervaloTentativas", $_POST['intervaloTentativas']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "SalvarArq", $_POST['SalvarArq']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "SepararPorMes", $_POST['SepararPorMes']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "AdicionarLiteral", $_POST['AdicionarLiteral']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "SepararPorCNPJ", $_POST['SepararPorCNPJ']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "SepararPorModelo", $_POST['SepararPorModelo']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "PathReinf", $_POST['PathReinf']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "IdContribuinte", $_POST['IdContribuinte']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "IdTransmissor", $_POST['IdTransmissor']) != 0) exit;
        if (ConfigGravarValor($ffi, "Reinf", "TipoContribuinte", $_POST['TipoContribuinte']) != 0) exit;

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

        $resultado = "Configurações salvas com sucesso.";
    }

    if ($metodo == "carregarConfiguracoes") {
        $processo = $metodo . "/" . "Reinf_ConfigLer";

        if (ConfigLerValor($ffi, "Principal", "LogPath", $LogPath) != 0) exit;
        if (ConfigLerValor($ffi, "Principal", "LogNivel", $LogNivel) != 0) exit;

        if (ConfigLerValor($ffi, "Reinf", "ExibirErroSchema", $exibirErroSchema) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "FormatoAlerta", $formatoAlerta) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "FormaEmissao", $formaEmissao) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "VersaoDF", $versaoDF) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "RetirarAcentos", $retirarAcentos) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "SalvarGer", $SalvarGer) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "PathSalvar", $pathSalvar) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "PathSchemas", $pathSchemas) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "SSLType", $SSLType) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "Timeout", $timeout) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "Ambiente", $ambiente) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "Visualizar", $visualizar) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "SalvarWS", $SalvarWS) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "AjustaAguardaConsultaRet", $ajustaAguardaConsultaRet) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "AguardarConsultaRet", $aguardarConsultaRet) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "Tentativas", $tentativas) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "IntervaloTentativas", $intervaloTentativas) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "SalvarArq", $SalvarArq) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "SepararPorMes", $SepararPorMes) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "AdicionarLiteral", $AdicionarLiteral) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "SepararPorCNPJ", $SepararPorCNPJ) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "SepararPorModelo", $SepararPorModelo) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "PathReinf", $PathReinf) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "IdContribuinte", $IdContribuinte) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "IdTransmissor", $IdTransmissor) != 0) exit;
        if (ConfigLerValor($ffi, "Reinf", "TipoContribuinte", $TipoContribuinte) != 0) exit;

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

        $processo = $metodo . "/" . "responseData";
        $responseData = [
            'dados' => [
                'LogPath' => $LogPath ?? '',
                'LogNivel' => $LogNivel ?? '',

                'exibirErroSchema' => $exibirErroSchema ?? '',
                'formatoAlerta' => $formatoAlerta ?? '',
                'formaEmissao' => $formaEmissao ?? '',
                'versaoDF' => $versaoDF ?? '',
                'retirarAcentos' => $retirarAcentos ?? '',
                'SalvarGer' => $SalvarGer ?? '',
                'pathSalvar' => $pathSalvar ?? '',
                'pathSchemas' => $pathSchemas ?? '',
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
                'SepararPorCNPJ' => $SepararPorCNPJ ?? '',
                'SepararPorModelo' => $SepararPorModelo ?? '',
                'PathReinf' => $PathReinf ?? '',
                'IdContribuinte' => $IdContribuinte ?? '',
                'IdTransmissor' => $IdTransmissor ?? '',
                'TipoContribuinte' => $TipoContribuinte ?? '',

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
                'NumeroSerie' => $NumeroSerie ?? ''
            ]
        ];
    }

    if ($metodo == "OpenSSLInfo") {
        $processo = "Reinf_OpenSSLInfo";

        if (OpenSSLInfo($ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ObterCertificados") {
        $processo = "Reinf_ObterCertificados";

        if (ObterCertificados($ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "CriarEnviarReinfIni") {
        $processo = "Reinf_CriarEnviarReinfIni";

        if (CriarEnviarReinf($ffi, $_POST['eArqIni'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarProtocolo") {
        $processo = "Reinf_ConsultarProtocolo";

        if (ConsultarProtocolo($ffi, $_POST['eProtocolo'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarReciboReinf") {
        $processo = "Reinf_ConsultarReciboReinf";

        if (ConsultarReciboReinf(
                                 $ffi, 
                                 $_POST['ePerApur'], 
                                 $_POST['aTipoEvento'], 
                                 $_POST['eNrInscEstab'], 
                                 $_POST['eCnpjPrestador'], 
                                 $_POST['eNrInscTomador'], 
                                 $_POST['eDtApur'], 
                                 $_POST['eCpfCnpjBenef'], 
                                 $_POST['eCnpjFonte'], 
                                 $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "CriarEnviarReinfXml") {
        $processo = "Reinf_CarregarXMLEventoReinf";

        if (CarregarXMLEventoReinf($ffi, $_POST['eArquivoOuXML'], $resultado) != 0) {
            exit;
        }

        $processo = "Reinf_EnviarReinf";

        if (EnviarReinf($ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ValidarXml") {
        $processo = "Reinf_ValidarXml";

        if (CarregarXMLEventoReinf($ffi, $_POST['eArquivoXML'], $resultado) != 0) {
            exit;
        }

        $processo = "Reinf_Validar";

        if (Validar($ffi, $resultado) != 0) {
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
    if ($processo != "Reinf_Inicializar") {
        $processo = "Reinf_Finalizar";
        if (Finalizar($ffi) != 0)
            exit;
    }
} catch (Exception $e) {
    $erro = $e->getMessage();
    echo json_encode(["mensagem" => "Exceção[$processo]: $erro"]);
    exit;
}

echo json_encode($responseData);

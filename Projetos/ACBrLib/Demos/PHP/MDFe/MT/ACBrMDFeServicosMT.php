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

include 'ACBrMDFeMT.php';
include '../../ACBrComum/ACBrComum.php';

$nomeLib = "ACBrMDFe";
$metodo = $_POST['metodo'];

if (ValidaFFI() != 0)
    exit;

$dllPath = CarregaDll(__DIR__, $nomeLib);

if ($dllPath == -10)
    exit;

$importsPath = CarregaImports(__DIR__, $nomeLib, 'MT');

if ($importsPath == -10)
    exit;

$iniPath = CarregaIniPath(__DIR__, $nomeLib);

$processo = "file_get_contents";
$ffi = CarregaContents($importsPath, $dllPath);
$handle = FFI::new("uintptr_t");

try {
    $resultado = "";
    $processo = "Inicializar";

    $processo = "MDFE_Inicializar";
    if (Inicializar($handle, $ffi, $iniPath) != 0)
        exit;

    if ($metodo == "salvarConfiguracoes") {
        $processo = $metodo . "/" . "MDFE_ConfigGravarValor";

        if (ConfigGravarValor($handle, $ffi, "Principal", "LogPath", $_POST['LogPath']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Principal", "LogNivel", $_POST['LogNivel']) != 0) exit;

        if (ConfigGravarValor($handle, $ffi, "MDFe", "ExibirErroSchema", $_POST['exibirErroSchema']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "FormatoAlerta", $_POST['formatoAlerta']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "FormaEmissao", $_POST['formaEmissao']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "VersaoDF", $_POST['versaoDF']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "RetirarAcentos", $_POST['retirarAcentos']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "SalvarGer", $_POST['SalvarGer']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "PathSalvar", $_POST['pathSalvar']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "PathSchemas", $_POST['pathSchemas']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "SSLType", $_POST['SSLType']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "Timeout", $_POST['timeout']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "Ambiente", $_POST['ambiente']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "Visualizar", $_POST['visualizar']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "SalvarWS", $_POST['SalvarWS']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "AjustaAguardaConsultaRet", $_POST['ajustaAguardaConsultaRet']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "AguardarConsultaRet", $_POST['aguardarConsultaRet']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "Tentativas", $_POST['tentativas']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "IntervaloTentativas", $_POST['intervaloTentativas']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "SepararPorMes", $_POST['SepararPorMes']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "AdicionarLiteral", $_POST['AdicionarLiteral']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "EmissaoPathMDFe", $_POST['EmissaoPathMDFe']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "SalvarArq", $_POST['SalvarArq']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "SalvarEvento", $_POST['SalvarEvento']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "SepararPorCNPJ", $_POST['SepararPorCNPJ']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "SepararPorModelo", $_POST['SepararPorModelo']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "PathMDFe", $_POST['PathMDFe']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "MDFe", "PathEvento", $_POST['PathEvento']) != 0) exit;

        if (ConfigGravarValor($handle, $ffi, "Proxy", "Servidor", $_POST['proxyServidor']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Proxy", "Porta", $_POST['proxyPorta']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Proxy", "Usuario", $_POST['proxyUsuario']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Proxy", "Senha", $_POST['proxySenha']) != 0) exit;

        if (ConfigGravarValor($handle, $ffi, "DFe", "UF", $_POST['UF']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DFe", "SSLCryptLib", $_POST['SSLCryptLib']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DFe", "SSLHttpLib", $_POST['SSLHttpLib']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DFe", "SSLXmlSignLib", $_POST['SSLXmlSignLib']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DFe", "ArquivoPFX", $_POST['ArquivoPFX']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DFe", "DadosPFX", $_POST['DadosPFX']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DFe", "Senha", $_POST['senhaCertificado']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DFe", "NumeroSerie", $_POST['NumeroSerie']) != 0) exit;

        if (ConfigGravarValor($handle, $ffi, "DAMDFe", "PathLogo", $_POST['PathLogo']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DAMDFe", "TipoDAMDFe", $_POST['TipoDAMDFe']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DAMDFe", "PathPDF", $_POST['PathMDFe']) != 0) exit;

        if (ConfigGravarValor($handle, $ffi, "Email", "Nome", $_POST['emailNome']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Email", "Conta", $_POST['emailConta']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Email", "Servidor", $_POST['emailServidor']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Email", "Porta", $_POST['emailPorta']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Email", "SSL", $_POST['emailSSL']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Email", "TLS", $_POST['emailTLS']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Email", "Usuario", $_POST['emailUsuario']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Email", "Senha", $_POST['emailSenha']) != 0) exit;

        $resultado = "Configurações salvas com sucesso.";
    }

    if ($metodo == "carregarConfiguracoes") {
        $processo = $metodo . "/" . "MDFE_ConfigLer";

        if (ConfigLerValor($handle, $ffi, "Principal", "LogPath", $LogPath) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Principal", "LogNivel", $LogNivel) != 0) exit;

        if (ConfigLerValor($handle, $ffi, "MDFe", "ExibirErroSchema", $exibirErroSchema) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "FormatoAlerta", $formatoAlerta) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "FormaEmissao", $formaEmissao) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "VersaoDF", $versaoDF) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "RetirarAcentos", $retirarAcentos) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "SalvarGer", $SalvarGer) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "PathSalvar", $pathSalvar) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "PathSchemas", $pathSchemas) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "SSLType", $SSLType) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "Timeout", $timeout) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "Ambiente", $ambiente) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "Visualizar", $visualizar) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "SalvarWS", $SalvarWS) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "AjustaAguardaConsultaRet", $ajustaAguardaConsultaRet) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "AguardarConsultaRet", $aguardarConsultaRet) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "Tentativas", $tentativas) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "IntervaloTentativas", $intervaloTentativas) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "SepararPorMes", $SepararPorMes) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "AdicionarLiteral", $AdicionarLiteral) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "EmissaoPathMDFe", $EmissaoPathMDFe) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "SalvarArq", $SalvarArq) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "SalvarEvento", $SalvarEvento) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "SepararPorCNPJ", $SepararPorCNPJ) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "SepararPorModelo", $SepararPorModelo) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "PathMDFe", $PathMDFe) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "MDFe", "PathEvento", $PathEvento) != 0) exit;

        if (ConfigLerValor($handle, $ffi, "Proxy", "Servidor", $proxyServidor) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Proxy", "Porta", $proxyPorta) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Proxy", "Usuario", $proxyUsuario) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Proxy", "Senha", $proxySenha) != 0) exit;

        if (ConfigLerValor($handle, $ffi, "DFe", "UF", $UF) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DFe", "SSLCryptLib", $SSLCryptLib) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DFe", "SSLHttpLib", $SSLHttpLib) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DFe", "SSLXmlSignLib", $SSLXmlSignLib) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DFe", "ArquivoPFX", $ArquivoPFX) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DFe", "DadosPFX", $DadosPFX) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DFe", "Senha", $senhaCertificado) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DFe", "NumeroSerie", $NumeroSerie) != 0) exit;

        if (ConfigLerValor($handle, $ffi, "DAMDFe", "PathLogo", $PathLogo) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DAMDFe", "TipoDAMDFe", $TipoDAMDFe) != 0) exit;

        if (ConfigLerValor($handle, $ffi, "Email", "Nome", $emailNome) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Email", "Conta", $emailConta) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Email", "Servidor", $emailServidor) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Email", "Porta", $emailPorta) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Email", "SSL", $emailSSL) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Email", "TLS", $emailTLS) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Email", "Usuario", $emailUsuario) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Email", "Senha", $emailSenha) != 0) exit;

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
                'SepararPorMes' => $SepararPorMes ?? '',
                'AdicionarLiteral' => $AdicionarLiteral ?? '',
                'EmissaoPathMDFe' => $EmissaoPathMDFe ?? '',
                'SalvarArq' => $SalvarArq ?? '',
                'SalvarEvento' => $SalvarEvento ?? '',
                'SepararPorCNPJ' => $SepararPorCNPJ ?? '',
                'SepararPorModelo' => $SepararPorModelo ?? '',
                'PathMDFe' => $PathMDFe ?? '',
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
                'TipoDAMDFe' => $TipoDAMDFe ?? '',

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

    if ($metodo == "OpenSSLInfo") {
        $processo = "MDFE_OpenSSLInfo";

        if (OpenSSLInfo($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ObterCertificados") {
        $processo = "MDFE_ObterCertificados";

        if (ObterCertificados($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "statusServico") {
        $processo = "MDFE_StatusServico";

        if (StatusServico($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "CarregarXmlMDFe") {
        $processo = "MDFE_CarregarXml";

        if (CarregarXmlMDFe($handle, $ffi, $_POST['conteudoArquivo01'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "CarregarIniMDFe") {
        $processo = "MDFE_CarregarINI";

        if (CarregarINI($handle, $ffi, $_POST['conteudoArquivo01'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "Enviar") {
        if ($_POST['tipoArquivo'] == "xml") {
            $processo = "MDFE_CarregarXml";

            if (CarregarXmlMDFe($handle, $ffi, $_POST['AeArquivoMDFe'], $resultado) != 0) {
                exit;
            }
        } else {
            $processo = "MDFE_CarregarINI";

            if (CarregarINI($handle, $ffi, $_POST['AeArquivoMDFe'], $resultado) != 0) {
                exit;
            }
        }

        $processo = "MDFE_AssinarMDFe";

        if (AssinarMDFe($handle, $ffi, $resultado) != 0) {
            exit;
        }

        $processo = "MDFE_Enviar";

        if (Enviar(
            $handle,
            $ffi,
            $_POST['ALote'],
            $_POST['AImprimir'],
            $_POST['ASincrono'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "ImprimirPDF") {
        $processo = "MDFE_CarregarXml";

        if (CarregarXmlMDFe($handle, $ffi, $_POST['AeArquivoXmlMDFe'], $resultado) != 0) {
            exit;
        }

        $processo = "MDFE_ImprimirPDF";

        if (ImprimirPDF($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "SalvarPDF") {
        $processo = "MDFE_CarregarXml";

        if (CarregarXmlMDFe($handle, $ffi, $_POST['AeArquivoXmlMDFe'], $resultado) != 0) {
            exit;
        }

        $processo = "MDFE_SalvarPDF";

        if (SalvarPDF($handle, $ffi, $resultado) != 0) {
            exit;
        }

        file_put_contents("teste.pdf", base64_decode($resultado));
    }

    if ($metodo == "ValidarRegrasdeNegocios") {
        $processo = "MDFE_CarregarXml";

        if (CarregarXmlMDFe($handle, $ffi, $_POST['AeArquivoXmlMDFe'], $resultado) != 0) {
            exit;
        }

        $processo = "MDFE_ValidarRegrasdeNegocios";

        if (ValidarRegrasdeNegocios($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "GerarChave") {
        $processo = "MDFE_GerarChave";

        if (GerarChave(
            $handle,
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

    if ($metodo == "EnviarEmail") {
        $processo = "MDFE_CarregarXml";

        if (CarregarXmlMDFe($handle, $ffi, $_POST['AeArquivoXmlMDFe'], $resultado) != 0) {
            exit;
        }

        $processo = "MDFE_EnviarEmail";

        if (EnviarEmail($handle, $ffi, $_POST['AePara'], $_POST['AeArquivoXmlMDFe'], $_POST['AEnviaPDF'], $_POST['AeAssunto'], $_POST['AeCC'], $_POST['AeAnexos'], $_POST['AeMensagem'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "Consultar") {
        $processo = "MDFE_Consultar";

        if (Consultar($handle, $ffi, $_POST['eChaveOuMDFe'], $_POST['AExtrairEventos'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarRecibo") {
        $processo = "MDFE_ConsultarRecibo";

        if (ConsultarRecibo($handle, $ffi, $_POST['ARecibo'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultaMDFeNaoEnc") {
        $processo = "MDFE_ConsultaMDFeNaoEnc";

        if (ConsultaMDFeNaoEnc($handle, $ffi, $_POST['nCNPJ'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "Cancelar") {
        $processo = "MDFE_Cancelar";

        if (Cancelar($handle, $ffi, $_POST['AeChave'], $_POST['AeJustificativa'], $_POST['AeCNPJCPF'], $_POST['ALote'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "EncerrarMDFe") {
        $processo = "MDFE_EncerrarMDFe";

        if (EncerrarMDFe($handle, $ffi, $_POST['AeChaveOuMDFe'], $_POST['AeDtEnc'], $_POST['AcMunicipioDescarga'], $_POST['AnCNPJ'], $_POST['AnProtocolo'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "InclusaoCondutor") {
        $processo = "MDFE_CarregarEventoINI";

        if (CarregarEventoINI($handle, $ffi, $_POST['AeArquivoIniEvento'], $resultado) != 0) {
            exit;
        }

        $processo = "MDFE_InclusaoCondutor";

        if (EnviarEvento($handle, $ffi, $_POST['AidLote'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ImprimirEventoPDF") {
        $processo = "MDFE_ImprimirEventoPDF";

        if (ImprimirEventoPDF($handle, $ffi, $_POST['AeArquivoXmlMDFe'], $_POST['AeArquivoXmlEvento'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "SalvarEventoPDF") {
        $processo = "MDFE_SalvarEventoPDF";

        if (SalvarEventoPDF($handle, $ffi, $_POST['AeArquivoXmlMDFe'], $_POST['AeArquivoXmlEvento'], $resultado) != 0) {
            exit;
        }

        file_put_contents("teste.pdf", base64_decode($resultado));
    }

    if ($metodo == "EnviarEmailEvento") {
        $processo = "MDFE_EnviarEmailEvento";

        if (EnviarEmailEvento($handle, $ffi, $_POST['AePara'], $_POST['eArquivoXmlEvento'], $_POST['eArquivoXmlMDFe'], $_POST['AEnviaPDF'], $_POST['AeAssunto'], $_POST['AeCC'], $_POST['AeAnexos'], $_POST['AeMensagem'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "DistribuicaoDFePorChave") {
        $processo = "MDFE_DistribuicaoDFePorChave";

        if (DistribuicaoDFePorChave($handle, $ffi, $_POST['AeCNPJCPF'], $_POST['AechMDFe'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "DistribuicaoDFePorNSU") {
        $processo = "MDFE_DistribuicaoDFePorNSU";

        if (DistribuicaoDFePorNSU($handle, $ffi, $_POST['AeCNPJCPF'], $_POST['AeNSU'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "DistribuicaoDFePorUltNSU") {
        $processo = "MDFE_DistribuicaoDFePorUltNSU";

        if (DistribuicaoDFePorUltNSU($handle, $ffi, $_POST['AeCNPJCPF'], $_POST['AeultNSU'], $resultado) != 0) {
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
    if ($processo != "MDFE_Inicializar") {
        $processo = "MDFE_Finalizar";
        if (Finalizar($handle, $ffi) != 0)
            exit;
    }
} catch (Exception $e) {
    $erro = $e->getMessage();
    echo json_encode(["mensagem" => "Exceção[$processo]: $erro"]);
    exit;
}

echo json_encode($responseData);

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

include 'ACBrCTeMT.php';
include '../../ACBrComum/ACBrComum.php';

$nomeLib = "ACBrCTe";
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

    $processo = "CTE_Inicializar";
    if (Inicializar($handle, $ffi, $iniPath) != 0)
        exit;

    if ($metodo == "salvarConfiguracoes") {
        $processo = $metodo . "/" . "CTE_ConfigGravarValor";

        if (ConfigGravarValor($handle, $ffi, "Principal", "LogPath", $_POST['LogPath']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Principal", "LogNivel", $_POST['LogNivel']) != 0) exit;

        if (ConfigGravarValor($handle, $ffi, "CTe", "ExibirErroSchema", $_POST['exibirErroSchema']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "FormatoAlerta", $_POST['formatoAlerta']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "FormaEmissao", $_POST['formaEmissao']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "ModeloDF", $_POST['modeloDF']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "VersaoDF", $_POST['versaoDF']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "RetirarAcentos", $_POST['retirarAcentos']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "SalvarGer", $_POST['SalvarGer']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "PathSalvar", $_POST['pathSalvar']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "PathSchemas", $_POST['pathSchemas']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "SSLType", $_POST['SSLType']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "Timeout", $_POST['timeout']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "Ambiente", $_POST['ambiente']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "Visualizar", $_POST['visualizar']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "SalvarWS", $_POST['SalvarWS']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "AjustaAguardaConsultaRet", $_POST['ajustaAguardaConsultaRet']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "AguardarConsultaRet", $_POST['aguardarConsultaRet']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "Tentativas", $_POST['tentativas']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "IntervaloTentativas", $_POST['intervaloTentativas']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "SalvarArq", $_POST['SalvarArq']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "SepararPorMes", $_POST['SepararPorMes']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "AdicionarLiteral", $_POST['AdicionarLiteral']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "EmissaoPathCTe", $_POST['EmissaoPathCTe']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "SepararPorCNPJ", $_POST['SepararPorCNPJ']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "SepararPorModelo", $_POST['SepararPorModelo']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "PathCTe", $_POST['PathCTe']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "PathInu", $_POST['PathInu']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CTe", "PathEvento", $_POST['PathEvento']) != 0) exit;

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

        if (ConfigGravarValor($handle, $ffi, "DACTE", "PathLogo", $_POST['PathLogo']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DACTE", "TipoDACTe", $_POST['TipoDACTe']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DACTE", "PathPDF", $_POST['PathPDF']) != 0) exit;

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
        $processo = $metodo . "/" . "CTE_ConfigLer";

        if (ConfigLerValor($handle, $ffi, "Principal", "LogPath", $LogPath) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Principal", "LogNivel", $LogNivel) != 0) exit;

        if (ConfigLerValor($handle, $ffi, "CTe", "ExibirErroSchema", $exibirErroSchema) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "FormatoAlerta", $formatoAlerta) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "FormaEmissao", $formaEmissao) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "ModeloDF", $modeloDF) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "VersaoDF", $versaoDF) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "RetirarAcentos", $retirarAcentos) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "SalvarGer", $SalvarGer) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "PathSalvar", $pathSalvar) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "PathSchemas", $pathSchemas) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "SSLType", $SSLType) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "Timeout", $timeout) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "Ambiente", $ambiente) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "Visualizar", $visualizar) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "SalvarWS", $SalvarWS) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "AjustaAguardaConsultaRet", $ajustaAguardaConsultaRet) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "AguardarConsultaRet", $aguardarConsultaRet) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "Tentativas", $tentativas) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "IntervaloTentativas", $intervaloTentativas) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "SalvarArq", $SalvarArq) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "SepararPorMes", $SepararPorMes) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "AdicionarLiteral", $AdicionarLiteral) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "EmissaoPathCTe", $EmissaoPathCTe) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "SepararPorCNPJ", $SepararPorCNPJ) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "SepararPorModelo", $SepararPorModelo) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "PathCTe", $PathCTe) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "PathInu", $PathInu) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CTe", "PathEvento", $PathEvento) != 0) exit;

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

        if (ConfigLerValor($handle, $ffi, "DACTE", "PathLogo", $PathLogo) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DACTE", "TipoDACTe", $TipoDACTe) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DACTE", "PathPDF", $PathPDF) != 0) exit;

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
                'modeloDF' => $modeloDF ?? '',
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
                'EmissaoPathCTe' => $EmissaoPathCTe ?? '',
                'SepararPorCNPJ' => $SepararPorCNPJ ?? '',
                'SepararPorModelo' => $SepararPorModelo ?? '',
                'PathCTe' => $PathCTe ?? '',
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
                'TipoDACTe' => $TipoDACTe ?? '',
                'PathPDF' => $PathPDF ?? '',

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
        $processo = "CTE_StatusServico";

        if (StatusServico($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "OpenSSLInfo") {
        $processo = "CTE_OpenSSLInfo";

        if (OpenSSLInfo($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ObterCertificados") {
        $processo = "CTE_ObterCertificados";

        if (ObterCertificados($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "CarregarXmlCte") {
        $processo = "CTE_CarregarXml";

        if (CarregarXmlCte($handle, $ffi, $_POST['conteudoArquivo01'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "CarregarIniCte") {
        $processo = "CarregarIniCte";

        if (CarregarINI($handle, $ffi, $_POST['conteudoArquivo01'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "CarregarEventoXML") {
        $processo = "CTE_CarregarEventoXml";

        if (CarregarEventoXML($handle, $ffi, $_POST['conteudoArquivo01'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "LimparListaCTe") {
        $processo = "CTE_LimparListaCTe";

        if (LimparListaCTe($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "LimparListaEventos") {
        $processo = "CTE_LimparListaEventos";

        if (LimparListaEventos($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "AssinarCTe") {
        $processo = "CTE_AssinarCTe";

        if (AssinarCTe($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ValidarCTe") {
        $processo = "CTE_ValidarCTe";

        if (ValidarCTe($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ValidarRegrasdeNegocios") {
        $processo = "CTE_CarregarXml";

        if (CarregarXmlCte($handle, $ffi, $_POST['AeArquivoXmlCTe'], $resultado) != 0) {
            exit;
        }

        $processo = "CTE_ValidarRegrasdeNegocios";

        if (ValidarRegrasdeNegocios($handle, $ffi, $resultado) != 0) {
            exit;
        }

        $resultado = "ok";
    }

    if ($metodo == "GerarChave") {
        $processo = "CTE_GerarChave";

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

    if ($metodo == "Consultar") {
        $processo = "CTE_Consultar";

        if (Consultar($handle, $ffi, $_POST['eChaveOuCTe'], $_POST['AExtrairEventos'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "Inutilizar") {
        $processo = "CTE_Inutilizar";

        if (Inutilizar(
            $handle,
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

    if ($metodo == "ImprimirInutilizacaoPDF") {
        $processo = "CTE_ImprimirInutilizacaoPDF";

        if (ImprimirInutilizacaoPDF($handle, $ffi, $_POST['AeArquivoXml'], $resultado) != 0) {
            exit;
        }

        $resultado = "ok";
    }

    if ($metodo == "Enviar") {
        if ($_POST['tipoArquivo'] == "xml") {
            $processo = "CTE_CarregarXml";

            if (CarregarXmlCte($handle, $ffi, $_POST['AeArquivoCTe'], $resultado) != 0) {
                exit;
            }
        } else {
            $processo = "CarregarIniCte";

            if (CarregarINI($handle, $ffi, $_POST['AeArquivoCTe'], $resultado) != 0) {
                exit;
            }
        }

        $processo = "CTE_AssinarCTe";

        if (AssinarCTe($handle, $ffi, $resultado) != 0) {
            exit;
        }

        $processo = "CTE_Enviar";

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

    if ($metodo == "ConsultarRecibo") {
        $processo = "CTE_ConsultarRecibo";

        if (ConsultarRecibo($handle, $ffi, $_POST['ARecibo'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "Cancelar") {
        $processo = "CTE_Cancelar";

        if (Cancelar($handle, $ffi, $_POST['AeChave'], $_POST['AeJustificativa'], $_POST['AeCNPJCPF'], $_POST['ALote'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "EnviarEvento") {
        $processo = "CTE_CarregarXml";

        if (CarregarXmlCte($handle, $ffi, $_POST['AeArquivoXmlCTe'], $resultado) != 0) {
            exit;
        }

        $processo = "CTE_CarregarEventoXml";

        if (CarregarEventoXML($handle, $ffi, $_POST['AeArquivoXmlEvento'], $resultado) != 0) {
            exit;
        }

        $processo = "CTE_EnviarEvento";

        if (EnviarEvento($handle, $ffi, $_POST['AidLote'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultaCadastro") {
        $processo = "CTE_ConsultaCadastro";

        if (ConsultaCadastro($handle, $ffi, $_POST['AcUF'], $_POST['AnDocumento'], $_POST['AnIE'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "DistribuicaoDFePorUltNSU") {
        $processo = "CTE_DistribuicaoDFePorUltNSU";

        if (DistribuicaoDFePorUltNSU($handle, $ffi, $_POST['AcUFAutor'], $_POST['AeCNPJCPF'], $_POST['AeultNSU'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "DistribuicaoDFePorNSU") {
        $processo = "CTE_DistribuicaoDFePorNSU";

        if (DistribuicaoDFePorNSU($handle, $ffi, $_POST['AcUFAutor'], $_POST['AeCNPJCPF'], $_POST['AeNSU'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "DistribuicaoDFePorChave") {
        $processo = "CTE_DistribuicaoDFePorChave";

        if (DistribuicaoDFePorChave($handle, $ffi, $_POST['AcUFAutor'], $_POST['AeCNPJCPF'], $_POST['AechCTe'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "EnviarEmail") {
        $processo = "CTE_CarregarXml";

        if (CarregarXmlCte($handle, $ffi, $_POST['AeArquivoXmlCTe'], $resultado) != 0) {
            exit;
        }

        $processo = "CTE_EnviarEmail";

        if (EnviarEmail($handle, $ffi, $_POST['AePara'], $_POST['AeChaveCTe'], $_POST['AEnviaPDF'], $_POST['AeAssunto'], $_POST['AeCC'], $_POST['AeAnexos'], $_POST['AeMensagem'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "EnviarEmailEvento") {
        $processo = "CTE_EnviarEmailEvento";

        if (EnviarEmailEvento($handle, $ffi, $_POST['AePara'], $_POST['AeChaveEvento'], $_POST['AeChaveCTe'], $_POST['AEnviaPDF'], $_POST['AeAssunto'], $_POST['AeCC'], $_POST['AeAnexos'], $_POST['AeMensagem'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ImprimirPDF") {
        $processo = "CTE_CarregarXml";

        if (CarregarXmlCte($handle, $ffi, $_POST['AeArquivoXmlCTe'], $resultado) != 0) {
            exit;
        }

        $processo = "CTE_ImprimirPDF";

        if (ImprimirPDF($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "SalvarPDF") {
        $processo = "CTE_CarregarXml";

        if (CarregarXmlCte($handle, $ffi, $_POST['AeArquivoXmlCTe'], $resultado) != 0) {
            exit;
        }

        $processo = "CTE_SalvarPDF";

        if (SalvarPDF($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ImprimirEventoPDF") {
        $processo = "CTE_ImprimirEventoPDF";

        if (ImprimirEventoPDF($handle, $ffi, $_POST['AeArquivoXmlCTe'], $_POST['AeArquivoXmlEvento'], $resultado) != 0) {
            exit;
        }

        $resultado = "ok";
    }

    if ($metodo == "SalvarEventoPDF") {
        $processo = "CTE_SalvarEventoPDF";

        if (SalvarEventoPDF($handle, $ffi, $_POST['AeArquivoXmlCTe'], $_POST['AeArquivoXmlEvento'], $resultado) != 0) {
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
    if ($processo != "CTE_Inicializar") {
        $processo = "CTE_Finalizar";
        if (Finalizar($handle, $ffi) != 0)
            exit;
    }
} catch (Exception $e) {
    $erro = $e->getMessage();
    echo json_encode(["mensagem" => "Exceção[$processo]: $erro"]);
    exit;
}

echo json_encode($responseData);

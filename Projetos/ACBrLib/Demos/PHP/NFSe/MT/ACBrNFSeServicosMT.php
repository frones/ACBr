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

include 'ACBrNFSeMT.php';
include '../../ACBrComum/ACBrComum.php';

$nomeLib = "ACBrNFSe";
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

    $processo = "NFSE_Inicializar";
    if (Inicializar($handle, $ffi, $iniPath) != 0)
        exit;

    if ($metodo == "salvarConfiguracoes") {
        $processo = $metodo . "/" . "NFSE_ConfigGravarValor";

        if (ConfigGravarValor($handle, $ffi, "Principal", "LogPath", $_POST['LogPath']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Principal", "LogNivel", $_POST['LogNivel']) != 0) exit;

        if (ConfigGravarValor($handle, $ffi, "NFSe", "ExibirErroSchema", $_POST['exibirErroSchema']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "FormatoAlerta", $_POST['formatoAlerta']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "FormaEmissao", $_POST['formaEmissao']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "RetirarAcentos", $_POST['retirarAcentos']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "SalvarGer", $_POST['SalvarGer']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "PathSalvar", $_POST['pathSalvar']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "MontarPathSchema", $_POST['MontarPathSchema']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "PathSchemas", $_POST['pathSchemas']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "IniServicos", $_POST['IniServicos']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "ConsultaLoteAposEnvio", $_POST['ConsultaLoteAposEnvio']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "ConsultaAposCancelar", $_POST['ConsultaAposCancelar']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "LayoutNFSe", $_POST['LayoutNFSe']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "SSLType", $_POST['SSLType']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Timeout", $_POST['timeout']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Ambiente", $_POST['ambiente']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Visualizar", $_POST['visualizar']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "SalvarWS", $_POST['SalvarWS']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Emitente.WSUser", $_POST['WSUser']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Emitente.WSSenha", $_POST['WSSenha']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Emitente.WSFraseSecr", $_POST['WSFraseSecr']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Emitente.WSChaveAcesso", $_POST['WSChaveAcesso']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Emitente.WSChaveAutoriz", $_POST['WSChaveAutoriz']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Emitente.CNPJ", $_POST['EmitenteCNPJ']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Emitente.InscMun", $_POST['EmitenteInscMun']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Emitente.RazSocial", $_POST['EmitenteRazSocial']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Emitente.Dados.NomeFantasia", $_POST['EmitenteNomeFantasia']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Emitente.Dados.Telefone", $_POST['EmitenteTelefone']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Emitente.Dados.CEP", $_POST['EmitenteCEP']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Emitente.Dados.Endereco", $_POST['EmitenteEndereco']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Emitente.Dados.Numero", $_POST['EmitenteNumero']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Emitente.Dados.Complemento", $_POST['EmitenteComplemento']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Emitente.Dados.Bairro", $_POST['EmitenteBairro']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "CodigoMunicipio", $_POST['CodigoMunicipio']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "CNPJPrefeitura", $_POST['CNPJPrefeitura']) != 0) exit;

        if (ConfigGravarValor($handle, $ffi, "NFSe", "AjustaAguardaConsultaRet", $_POST['ajustaAguardaConsultaRet']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "AguardarConsultaRet", $_POST['aguardarConsultaRet']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "Tentativas", $_POST['tentativas']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "IntervaloTentativas", $_POST['intervaloTentativas']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "SalvarArq", $_POST['SalvarArq']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "SepararPorMes", $_POST['SepararPorMes']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "AdicionarLiteral", $_POST['AdicionarLiteral']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "EmissaoPathNFSe", $_POST['EmissaoPathNFSe']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "NFSe", "SepararPorCNPJ", $_POST['SepararPorCNPJ']) != 0) exit;

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

        if (ConfigGravarValor($handle, $ffi, "DANFSe", "PathLogo", $_POST['PathLogo']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DANFSe", "Prestador.Logo", $_POST['PrestadorLogo']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DANFSe", "Prefeitura", $_POST['Prefeitura']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DANFSe", "PathPDF", $_POST['PathPDF']) != 0) exit;

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
        $processo = $metodo . "/" . "NFSE_ConfigLer";

        if (ConfigLerValor($handle, $ffi, "Principal", "LogPath", $LogPath) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Principal", "LogNivel", $LogNivel) != 0) exit;

        if (ConfigLerValor($handle, $ffi, "NFSe", "ExibirErroSchema", $exibirErroSchema) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "FormatoAlerta", $formatoAlerta) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "FormaEmissao", $formaEmissao) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "RetirarAcentos", $retirarAcentos) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "SalvarGer", $SalvarGer) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "PathSalvar", $pathSalvar) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "MontarPathSchema", $MontarPathSchema) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "PathSchemas", $pathSchemas) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "IniServicos", $IniServicos) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "ConsultaLoteAposEnvio", $ConsultaLoteAposEnvio) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "ConsultaAposCancelar", $ConsultaAposCancelar) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "LayoutNFSe", $LayoutNFSe) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "SSLType", $SSLType) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Timeout", $timeout) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Ambiente", $ambiente) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Visualizar", $visualizar) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "SalvarWS", $SalvarWS) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Emitente.WSUser", $WSUser) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Emitente.WSSenha", $WSSenha) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Emitente.WSFraseSecr", $WSFraseSecr) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Emitente.WSChaveAcesso", $WSChaveAcesso) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Emitente.WSChaveAutoriz", $WSChaveAutoriz) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Emitente.CNPJ", $EmitenteCNPJ) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Emitente.InscMun", $EmitenteInscMun) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Emitente.RazSocial", $EmitenteRazSocial) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Emitente.Dados.NomeFantasia", $EmitenteNomeFantasia) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Emitente.Dados.Telefone", $EmitenteTelefone) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Emitente.Dados.CEP", $EmitenteCEP) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Emitente.Dados.Endereco", $EmitenteEndereco) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Emitente.Dados.Numero", $EmitenteNumero) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Emitente.Dados.Complemento", $EmitenteComplemento) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Emitente.Dados.Bairro", $EmitenteBairro) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "CodigoMunicipio", $CodigoMunicipio) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "CNPJPrefeitura", $CNPJPrefeitura) != 0) exit;

        if (ConfigLerValor($handle, $ffi, "NFSe", "AjustaAguardaConsultaRet", $ajustaAguardaConsultaRet) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "AguardarConsultaRet", $aguardarConsultaRet) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "Tentativas", $tentativas) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "IntervaloTentativas", $intervaloTentativas) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "SalvarArq", $SalvarArq) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "SepararPorMes", $SepararPorMes) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "AdicionarLiteral", $AdicionarLiteral) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "EmissaoPathNFSe", $EmissaoPathNFSe) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "NFSe", "SepararPorCNPJ", $SepararPorCNPJ) != 0) exit;

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

        if (ConfigLerValor($handle, $ffi, "DANFSe", "PathLogo", $PathLogo) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DANFSe", "Prestador.Logo", $PrestadorLogo) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DANFSe", "Prefeitura", $Prefeitura) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DANFSe", "PathPDF", $PathPDF) != 0) exit;

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
                'retirarAcentos' => $retirarAcentos ?? '',
                'SalvarGer' => $SalvarGer ?? '',
                'pathSalvar' => $pathSalvar ?? '',
                'MontarPathSchema' => $MontarPathSchema ?? '',
                'pathSchemas' => $pathSchemas ?? '',
                'IniServicos' => $IniServicos ?? '',
                'ConsultaLoteAposEnvio' => $ConsultaLoteAposEnvio ?? '',
                'ConsultaAposCancelar' => $ConsultaAposCancelar ?? '',
                'LayoutNFSe' => $LayoutNFSe ?? '',
                'SSLType' => $SSLType ?? '',
                'timeout' => $timeout ?? '',
                'ambiente' => $ambiente ?? '',
                'visualizar' => $visualizar ?? '',
                'SalvarWS' => $SalvarWS ?? '',
                'WSUser' => $WSUser ?? '',
                'WSSenha' => $WSSenha ?? '',
                'WSFraseSecr' => $WSFraseSecr ?? '',
                'WSChaveAcesso' => $WSChaveAcesso ?? '',
                'WSChaveAutoriz' => $WSChaveAutoriz ?? '',
                'EmitenteCNPJ' => $EmitenteCNPJ ?? '',
                'EmitenteInscMun' => $EmitenteInscMun ?? '',
                'EmitenteRazSocial' => $EmitenteRazSocial ?? '',
                'EmitenteNomeFantasia' => $EmitenteNomeFantasia ?? '',
                'EmitenteTelefone' => $EmitenteTelefone ?? '',
                'EmitenteCEP' => $EmitenteCEP ?? '',
                'EmitenteEndereco' => $EmitenteEndereco ?? '',
                'EmitenteNumero' => $EmitenteNumero ?? '',
                'EmitenteComplemento' => $EmitenteComplemento ?? '',
                'EmitenteBairro' => $EmitenteBairro ?? '',
                'CodigoMunicipio' => $CodigoMunicipio ?? '',
                'CNPJPrefeitura' => $CNPJPrefeitura ?? '',

                'ajustaAguardaConsultaRet' => $ajustaAguardaConsultaRet ?? '',
                'aguardarConsultaRet' => $aguardarConsultaRet ?? '',
                'tentativas' => $tentativas ?? '',
                'intervaloTentativas' => $intervaloTentativas ?? '',
                'SalvarArq' => $SalvarArq ?? '',
                'SepararPorMes' => $SepararPorMes ?? '',
                'AdicionarLiteral' => $AdicionarLiteral ?? '',
                'EmissaoPathNFSe' => $EmissaoPathNFSe ?? '',
                'SepararPorCNPJ' => $SepararPorCNPJ ?? '',

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
                'PrestadorLogo' => $PrestadorLogo ?? '',
                'Prefeitura' => $Prefeitura ?? '',
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

    if ($metodo == "OpenSSLInfo") {
        $processo = "NFSE_OpenSSLInfo";

        if (OpenSSLInfo($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    // Carrega ini ou xml
    function CarregaIniOuXml($handle, $ffi, $arquivo)
    {
        $xmlOuIni = VerificaXmlOuIni($arquivo);

        if ($xmlOuIni === 0) {
            if (CarregarINI($handle, $ffi, $arquivo, $resultado) != 0) {
                exit;
            }

            return "ok";
        } else if ($xmlOuIni === 1) {
            if (CarregarXML($handle, $ffi, $arquivo, $resultado) != 0) {
                exit;
            }

            return "ok";
        } else {
            return "Arquivo inválido";
        }
    }

    if ($metodo == "Emitir") {
        $processo = "NFSE_CarregarIniOuXml";
        $resultado = CarregaIniOuXml($handle, $ffi, $_POST['AeArquivoXmlOuIni']);

        if ($resultado === "ok") {
            $processo = "NFSE_Emitir";

            if (Emitir($handle, $ffi, $_POST['ALote'], $_POST['AModoEnvio'], $resultado) != 0) {
                exit;
            }
        }
    }

    if ($metodo == "ConsultarSituacao") {
        $processo = "NFSE_ConsultarSituacao";

        if (ConsultarSituacao($handle, $ffi, $_POST['AProtocolo'], $_POST['ANumLote'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarLoteRps") {
        $processo = "NFSE_ConsultarLoteRps";

        if (ConsultarLoteRps($handle, $ffi, $_POST['AProtocolo'], $_POST['ANumLote'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarNFSePorPeriodo") {
        $processo = "NFSE_ConsultarNFSePorPeriodo";

        if (ConsultarNFSePorPeriodo(
            $handle,
            $ffi,
            strDateToDoubleDate($_POST['ADataInicial']),
            strDateToDoubleDate($_POST['ADataFinal']),
            $_POST['APagina'],
            $_POST['ANumeroLote'],
            $_POST['ATipoPeriodo'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarNFSePorNumero") {
        $processo = "NFSE_ConsultarNFSePorNumero";

        if (ConsultarNFSePorNumero($handle, $ffi, $_POST['ANumero'], $_POST['APagina'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarNFSePorRps") {
        $processo = "NFSE_ConsultarNFSePorRps";

        if (ConsultarNFSePorRps(
            $handle,
            $ffi,
            $_POST['ANumeroRps'],
            $_POST['ASerie'],
            $_POST['ATipo'],
            $_POST['ACodigoVerificacao'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarNFSeGenerico") {
        $processo = "NFSE_ConsultarNFSeGenerico";

        if (ConsultarNFSeGenerico($handle, $ffi, $_POST['AInfConsultaNFSe'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarNFSePorFaixa") {
        $processo = "NFSE_ConsultarNFSePorFaixa";

        if (ConsultarNFSePorFaixa($handle, $ffi, $_POST['ANumeroInicial'], $_POST['ANumeroFinal'], $_POST['APagina'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarLinkNFSe") {
        $processo = "NFSE_ConsultarLinkNFSe";

        if (ConsultarLinkNFSe($handle, $ffi, $_POST['AInfConsultaLinkNFSe'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "EnviarEmail") {
        $processo = "NFSE_EnviarEmail";

        if (EnviarEmail(
            $handle,
            $ffi,
            $_POST['AePara'],
            $_POST['AeXmlNFSe'],
            $_POST['AEnviaPDF'],
            $_POST['AeAssunto'],
            $_POST['AeCC'],
            $_POST['AeAnexos'],
            $_POST['AeMensagem'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "LinkNFSe") {
        $processo = "NFSE_LinkNFSe";

        if (LinkNFSe(
            $handle,
            $ffi,
            $_POST['ANumeroNFSe'],
            $_POST['ACodigoVerificacao'],
            $_POST['AChaveAcesso'],
            $_POST['AValorServico'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "GerarToken") {
        $processo = "NFSE_GerarToken";

        if (GerarToken($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "SalvarPDF") {
        $processo = "NFSE_CarregarIniOuXml";
        $resultado = CarregaIniOuXml($handle, $ffi, $_POST['AeArquivoXml']);

        if ($resultado === "ok") {
            $processo = "NFSE_SalvarPDF";

            if (SalvarPDF($handle, $ffi, $resultado) != 0) {
                exit;
            }
        }
    }

    if ($metodo == "SubstituirNFSe") {
        $processo = "NFSE_CarregarIniOuXml";
        $resultado = CarregaIniOuXml($handle, $ffi, $_POST['AeArquivoXmlOuIni']);

        if ($resultado === "ok") {
            $processo = "NFSE_SubstituirNFSe";

            if (SubstituirNFSe(
                $handle,
                $ffi,
                $_POST['ANumeroNFSe'],
                $_POST['ASerieNFSe'],
                $_POST['ACodigoCancelamento'],
                $_POST['AMotivoCancelamento'],
                $_POST['ANumeroLote'],
                $_POST['ACodigoVerificacao'],
                $resultado
            ) != 0) {
                exit;
            }
        }
    }

    if ($metodo == "Cancelar") {
        $processo = "NFSE_Cancelar";

        if (Cancelar($handle, $ffi, $_POST['AInfCancelamentoNFSe'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarNFSeServicoPrestadoPorNumero") {
        $processo = "NFSE_ConsultarNFSeServicoPrestadoPorNumero";

        if (ConsultarNFSeServicoPrestadoPorNumero(
            $handle,
            $ffi,
            $_POST['ANumero'],
            $_POST['APagina'],
            strDateToDoubleDate($_POST['ADataInicial']),
            strDateToDoubleDate($_POST['ADataFinal']),
            $_POST['ATipoPeriodo'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarNFSeServicoPrestadoPorTomador") {
        $processo = "NFSE_ConsultarNFSeServicoPrestadoPorTomador";

        if (ConsultarNFSeServicoPrestadoPorTomador(
            $handle,
            $ffi,
            $_POST['ACNPJ'],
            $_POST['AInscMun'],
            $_POST['APagina'],
            strDateToDoubleDate($_POST['ADataInicial']),
            strDateToDoubleDate($_POST['ADataFinal']),
            $_POST['ATipoPeriodo'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarNFSeServicoPrestadoPorPeriodo") {
        $processo = "NFSE_ConsultarNFSeServicoPrestadoPorPeriodo";

        if (ConsultarNFSeServicoPrestadoPorPeriodo(
            $handle,
            $ffi,
            strDateToDoubleDate($_POST['ADataInicial']),
            strDateToDoubleDate($_POST['ADataFinal']),
            $_POST['APagina'],
            $_POST['ATipoPeriodo'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarNFSeServicoPrestadoPorIntermediario") {
        $processo = "NFSE_ConsultarNFSeServicoPrestadoPorIntermediario";

        if (ConsultarNFSeServicoPrestadoPorIntermediario(
            $handle,
            $ffi,
            $_POST['ACNPJ'],
            $_POST['AInscMun'],
            $_POST['APagina'],
            strDateToDoubleDate($_POST['ADataInicial']),
            strDateToDoubleDate($_POST['ADataFinal']),
            $_POST['ATipoPeriodo'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarNFSeServicoTomadoPorNumero") {
        $processo = "NFSE_ConsultarNFSeServicoTomadoPorNumero";

        if (ConsultarNFSeServicoTomadoPorNumero(
            $handle,
            $ffi,
            $_POST['ANumero'],
            $_POST['APagina'],
            strDateToDoubleDate($_POST['ADataInicial']),
            strDateToDoubleDate($_POST['ADataFinal']),
            $_POST['ATipoPeriodo'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarNFSeServicoTomadoPorPrestador") {
        $processo = "NFSE_ConsultarNFSeServicoTomadoPorPrestador";

        if (ConsultarNFSeServicoTomadoPorPrestador(
            $handle,
            $ffi,
            $_POST['ACNPJ'],
            $_POST['AInscMun'],
            $_POST['APagina'],
            strDateToDoubleDate($_POST['ADataInicial']),
            strDateToDoubleDate($_POST['ADataFinal']),
            $_POST['ATipoPeriodo'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarNFSeServicoTomadoPorTomador") {
        $processo = "NFSE_ConsultarNFSeServicoTomadoPorTomador";

        if (ConsultarNFSeServicoTomadoPorTomador(
            $handle,
            $ffi,
            $_POST['ACNPJ'],
            $_POST['AInscMun'],
            $_POST['APagina'],
            strDateToDoubleDate($_POST['ADataInicial']),
            strDateToDoubleDate($_POST['ADataFinal']),
            $_POST['ATipoPeriodo'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarNFSeServicoTomadoPorPeriodo") {
        $processo = "NFSE_ConsultarNFSeServicoTomadoPorPeriodo";

        if (ConsultarNFSeServicoTomadoPorPeriodo(
            $handle,
            $ffi,
            strDateToDoubleDate($_POST['ADataInicial']),
            strDateToDoubleDate($_POST['ADataFinal']),
            $_POST['APagina'],
            $_POST['ATipoPeriodo'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarNFSeServicoTomadoPorIntermediario") {
        $processo = "NFSE_ConsultarNFSeServicoTomadoPorIntermediario";

        if (ConsultarNFSeServicoTomadoPorIntermediario(
            $handle,
            $ffi,
            $_POST['ACNPJ'],
            $_POST['AInscMun'],
            $_POST['APagina'],
            strDateToDoubleDate($_POST['ADataInicial']),
            strDateToDoubleDate($_POST['ADataFinal']),
            $_POST['ATipoPeriodo'],
            $resultado
        ) != 0) {
            exit;
        }
    }

    if ($metodo == "EnviarEvento") {
        $processo = "NFSE_EnviarEvento";

        if (EnviarEvento($handle, $ffi, $_POST['AInfEvento'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarDPSPorChave") {
        $processo = "NFSE_ConsultarDPSPorChave";

        if (ConsultarDPSPorChave($handle, $ffi, $_POST['AChaveDPS'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarNFSePorChave") {
        $processo = "NFSE_ConsultarNFSePorChave";

        if (ConsultarNFSePorChave($handle, $ffi, $_POST['AChaveNFSe'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarEvento") {
        $processo = "NFSE_ConsultarEvento";

        if (ConsultarEvento($handle, $ffi, $_POST['AChave'], $_POST['ATipoEvento'], $_POST['ANumSeq'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ConsultarDFe") {
        $processo = "NFSE_ConsultarDFe";

        if (ConsultarDFe($handle, $ffi, $_POST['ANSU'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ObterDANFSE") {
        $processo = "NFSE_ObterDANFSE";

        if (ObterDANFSE($handle, $ffi, $_POST['AChaveNFSe'], $resultado) != 0) {
            exit;
        }

        $posicao = strpos($resultado, "XmlRetorno=");

        if ($posicao > 0){
            $conteudoDecodificado = substr($resultado, $posicao + 11, -1);
            $conteudoDecodificado = base64_decode($conteudoDecodificado);
    
            file_put_contents("teste.pdf", $conteudoDecodificado);
        }
    }

    if ($metodo == "ConsultarParametros") {
        $processo = "NFSE_ConsultarParametros";

        if (ConsultarParametros(
            $handle,
            $ffi,
            $_POST['ATipoParametroMunicipio'],
            $_POST['ACodigoServico'],
            strDateToDoubleDate($_POST['ACompetencia']),
            $_POST['ANumeroBeneficio'],
            $resultado
        ) != 0) {
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
    if ($processo != "NFSE_Inicializar") {
        $processo = "NFSE_Finalizar";
        if (Finalizar($handle, $ffi) != 0)
            exit;
    }
} catch (Exception $e) {
    $erro = $e->getMessage();
    echo json_encode(["mensagem" => "Exceção[$processo]: $erro"]);
    exit;
}

echo json_encode($responseData);

{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}
unit DoSATUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CmdUnit, pcnConversao, strutils,
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.Math,
  ACBrSAT, ACBrMonitorConfig, ACBrMonitorConsts, ACBrDFeUtil,
  ACBrLibSATRespostas, ACBrLibResposta,
  ACBrSATExtratoESCPOS, ACBrSATExtratoFortesFr, ACBrValidador, ACBrDFeSSL;

type

{ TACBrObjetoSAT }

TACBrObjetoSAT = class(TACBrObjetoDFe)
private
  fACBrSAT: TACBrSAT;
public
  constructor Create(AConfig: TMonitorConfig; ACBrSAT: TACBrSAT); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  procedure RespostaConsultaSessao(ArqCFe: String);
  procedure RespostaConsultaSessaoCancelado(ArqCFe: String);
  procedure RespostaStatusSAT;
  procedure RespostaCriarCFe(ArqCFe: String);
  procedure RespostaEnviarDadosVenda( Resultado: String);
  procedure RespostaCancelarVenda( Resultado: String);
  procedure RespostaTesteFimaFim( Resultado: String );
  procedure RespostaPadrao;
  procedure RespostaPadraoCancelamento;
  procedure RespostaIntegrador;

  procedure CarregarDadosVenda(aStr: String; aNomePDF : String = '');
  procedure CarregarDadosCancelamento(aStr: String);

  procedure GerarIniCFe( AStr: String);

  property ACBrSAT: TACBrSAT read fACBrSAT;
end;

{ TMetodoAtivar }

TMetodoAtivar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoInicializar }

TMetodoInicializar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDesinicializar}

TMetodoDesinicializar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAssociarAssinatura}

TMetodoAssociarAssinatura = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoBloquear}

TMetodoBloquear = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDesbloquear}

TMetodoDesbloquear = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTrocarCodigoAtivacao}

TMetodoTrocarCodigoAtivacao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarSat }

TMetodoConsultarSat = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarUltimaSessaoFiscal }

TMetodoConsultarUltimaSessaoFiscal = class(TACBrMetodo)
public
  procedure Executar; override;
end;


{ TMetodoConsultarStatusOperacional }

TMetodoConsultarStatusOperacional = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarSessao }

TMetodoConsultarSessao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAtualizaSoftware }

TMetodoAtualizaSoftware = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoComunicarCertificado }

TMetodoComunicarCertificado = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCarregarDadosVenda }

TMetodoCarregarDadosVenda = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCarregarDadosCancelamento }

TMetodoCarregarDadosCancelamento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarCFe }

TMetodoCriarCFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarEnviarCFe }

TMetodoCriarEnviarCFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarCFe }

TMetodoEnviarCFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCancelarCFe }

TMetodoCancelarCFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirExtratoVenda }

TMetodoImprimirExtratoVenda = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirExtratoResumido }

TMetodoImprimirExtratoResumido = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirExtratoCancelamento }

TMetodoImprimirExtratoCancelamento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGerarImpressaoFiscalMFe }

TMetodoGerarImpressaoFiscalMFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGerarPDFExtratoVenda }

TMetodoGerarPDFExtratoVenda = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGerarPDFExtratoCancelamento }

TMetodoGerarPDFExtratoCancelamento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoExtrairLog }

TMetodoExtrairLog = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTesteFimaFim}

TMetodoTesteFimaFim = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetNumeroSessao}

TMetodoSetNumeroSessao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetLogoMarca}

TMetodoSetLogoMarca = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGerarAssinaturaSAT }

TMetodoGerarAssinaturaSAT = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarEmailCFe }

TMetodoEnviarEmailCFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarModeloSAT }

TMetodoConsultarModeloSAT = class(TACBrMetodo)
public
  procedure Executar; override;
end;


implementation

uses
  DoACBrUnit,IniFiles, forms, pcnAuxiliar, typinfo,
  ACBrSATExtratoClass, UtilUnit;

{ TMetodoGerarPDFExtratoCancelamento }

{ Params: 0 - cXMLVenda: String com XML de Vendas o path do arquivo
          1 - cXMLCancelamento: String com XML de Cancelamento o path do arquivo
          2 - cNomeArq: String com nome do o Arquivo
}
procedure TMetodoGerarPDFExtratoCancelamento.Executar;
var
  cXMLVenda : String;
  cXMLCancelamento: String;
  cNomeArq : String;
begin
  cXMLVenda := fpCmd.Params(0);
  cXMLCancelamento := fpCmd.Params(1);
  cNomeArq := fpCmd.Params(2);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    DoPrepararImpressaoSAT('', True);
    CarregarDadosVenda(cXMLVenda, cNomeArq);
    CarregarDadosCancelamento(cXMLCancelamento);
    ACBrSAT.ImprimirExtratoCancelamento;

    RespostaPadraoCancelamento;

  end;

end;

{ TMetodoConsultarModeloSAT }

procedure TMetodoConsultarModeloSAT.Executar;
  function SectionSAT(N: Integer): String;
  begin
    Result := 'SAT'+IntToStr(N);
  end;
var
  dfesat_ini, Marca: String;
  {$IfNDef MSWINDOWS}
  LibName: String;
  {$Else}
    {$IfDef CPU64}
    LibNameWin64: String;
    {$EndIf}
  {$EndIf}
  SL: TStringList;
  Ini: TIniFile;
  I: Integer;
begin
  dfesat_ini := PathWithDelim(ExtractFilePath(Application.ExeName)) + CDirSAT + PathDelim + CDFeSATIniFile;
  if not FileExists(dfesat_ini) then
    exit;

  Ini := TIniFile.Create(dfesat_ini);
  SL := TStringList.Create;
  try
    SL.LineBreak := '|';
    I := 1;
    while Ini.SectionExists(SectionSAT(I)) do
    begin
      Marca := Ini.ReadString(SectionSAT(I), CKeySATMarca, '');
      if Marca <> '' then
      begin
        {$IfNDef MSWINDOWS}
         LibName := Ini.ReadString(SectionSAT(I), CKeySATLibLinux, '');
         if LibName <> '' then
           SL.Add(Marca);
        {$Else}
          {$IfDef CPU64}
          LibNameWin64 := Ini.ReadString(SectionSAT(I), CKeySATLibWin64, '');
          if LibNameWin64 <> '' then
            SL.Add(Marca);
          {$Else}
            SL.Add(Marca);
          {$EndIf}
        {$EndIf}

      end;

      Inc( I );
    end;

    fpCmd.Resposta := SL.Text;
  finally
    SL.Free;
    Ini.Free;
  end;
end;

procedure TACBrObjetoSAT.CarregarDadosVenda(aStr: String; aNomePDF: String);
begin
  if Trim(aStr) = '' then
    raise Exception.Create('Nenhum arquivo informado como parâmetro! ');

  with fACBrSAT do
  begin
    CFe.Clear;
    if (pos(#10,aStr) = 0) and FileExists(aStr) then
    begin
      if not(CFe.LoadFromFile(aStr)) then
        raise Exception.Create('Falha ao carregar o arquivo '+aStr+'! ');
    end
    else
    if StringIsXML( aStr ) then
      CFe.AsXMLString := ConvertStrRecived(aStr)
    else
      raise Exception.Create('Diretório ou XML: '+aStr+' inválido! ');

    if (CFe.signature.DigestValue = '') then
      raise Exception.Create('Arquivo XML inválido, verifique se está passando o XML de retorno Assinado pelo SAT! ');

    if ( Extrato.Filtro = TACBrSATExtratoFiltro(fiPDF) ) then
      Extrato.NomeDocumento := IfThen(aNomePDF <> '', aNomePDF ,
        CalcCFeNomeArq(ConfigArquivos.PastaCFeVenda,CFe.infCFe.ID,'','.pdf'));
  end;

end;

procedure TACBrObjetoSAT.CarregarDadosCancelamento(aStr: String);
begin
  if Trim(aStr) = '' then
    raise Exception.Create('Nenhum arquivo informado como parâmetro! ');

  if (pos(#10,aStr) = 0) and FileExists(aStr) then
  begin
    if not(fACBrSAT.CFeCanc.LoadFromFile(aStr)) then
      raise Exception.Create('Falha ao carregar o arquivo '+aStr+'! ');
  end
  else
  if StringIsXML( aStr ) then
    fACBrSAT.CFeCanc.AsXMLString := ConvertStrRecived(aStr)
  else
    raise Exception.Create('Diretório ou XML: '+aStr+' inválido! ');

end;

procedure TACBrObjetoSAT.GerarIniCFe(AStr: String);
//var
  //ok: boolean;
begin
  with fACBrSAT do
  begin
    CFe.Clear;
    //Campos preenchidos em tela
    ACBrSAT.Config.infCFe_versaoDadosEnt := MonitorConfig.SAT.versaoDadosEnt;
    CFe.infCFe.versaoDadosEnt:= ACBrSAT.Config.infCFe_versaoDadosEnt;

    CFe.ide.CNPJ := MonitorConfig.SAT.SATSWH.CNPJ;
    CFe.ide.signAC := MonitorConfig.SAT.SATSWH.Assinatura;
    CFe.ide.numeroCaixa := MonitorConfig.SAT.NumeroCaixa;
    CFe.Emit.CNPJ := MonitorConfig.SAT.SATImpressao.SATEmit.CNPJ;
    CFe.Emit.IE := MonitorConfig.SAT.SATImpressao.SATEmit.IE;
    CFe.Emit.IM := MonitorConfig.SAT.SATImpressao.SATEmit.IM;
    CFe.Emit.cRegTrib := TpcnRegTrib(MonitorConfig.SAT.SATImpressao.SATEmit.RegTributario);
    CFe.Emit.cRegTribISSQN := TpcnRegTribISSQN(MonitorConfig.SAT.SATImpressao.SATEmit.RegTribISSQN);
    CFe.Emit.indRatISSQN :=  TpcnindRatISSQN(MonitorConfig.SAT.SATImpressao.SATEmit.IndRatISSQN);

    CFe.LoadFromIni(AStr);

  end;

end;

{ TMetodoGerarAssinaturaSAT }

{ Params: 0 - CNPJSHW : cnpj da Software House
          1 - CNPJEmitente : cnpj do Emitente
}
procedure TMetodoGerarAssinaturaSAT.Executar;
var
  cCNPJShw: String;
  cCNPJEmitente: String;
  cCodigoVinculacao: String;
  cMsgErroValidacao: String;

  procedure ConfiguraDFe;
  begin
    with TACBrObjetoSAT(fpObjetoDono) do
    begin
      case MonitorConfig.DFE.Certificado.CryptLib of
        1: fACBrSAT.SSL.SSLCryptLib := cryOpenSSL;
        2: fACBrSAT.SSL.SSLCryptLib := cryCapicom;
        3: fACBrSAT.SSL.SSLCryptLib := cryWinCrypt;
      else
        fACBrSAT.SSL.SSLCryptLib := cryWinCrypt;
      end;

      if NaoEstaVazio(MonitorConfig.DFE.Certificado.ArquivoPFX) then
        fACBrSAT.SSL.ArquivoPFX  := Trim(MonitorConfig.DFE.Certificado.ArquivoPFX )
      else
        fACBrSAT.SSL.NumeroSerie := Trim(MonitorConfig.DFE.Certificado.NumeroSerie );

      fACBrSAT.SSL.Senha       := Trim(MonitorConfig.DFE.Certificado.Senha );

    end;

  end;

begin
  cCNPJShw:= fpCmd.Params(0);
  cCNPJEmitente:= fpCmd.Params(1);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin

    if EstaVazio(cCNPJShw) then
      cCNPJShw:= Trim(MonitorConfig.SAT.SATSWH.CNPJ);

    if EstaVazio(cCNPJEmitente) then
      cCNPJEmitente:= Trim(MonitorConfig.SAT.SATImpressao.SATEmit.CNPJ);

    if EstaVazio(Trim(MonitorConfig.DFE.Certificado.NumeroSerie )) and
       EstaVazio(Trim(MonitorConfig.DFE.Certificado.ArquivoPFX ))then
      raise Exception.Create('Certificado não foi informado!');


    cMsgErroValidacao := ACBrValidador.ValidarCNPJ(cCNPJShw);
    if NaoEstaVazio( Trim(cMsgErroValidacao) ) then
      raise Exception.Create('CNPJ da Software House inválido!');


    cMsgErroValidacao := ACBrValidador.ValidarCNPJ(cCNPJEmitente);
    if Trim(cMsgErroValidacao) <> '' then
      raise Exception.Create('CNPJ do Emitente inválido!');

    ConfiguraDFe;

    cCodigoVinculacao := Onlynumber(cCNPJShw) + Onlynumber(cCNPJEmitente);
    fpCmd.Resposta := fACBrSAT.SSL.CalcHash(cCodigoVinculacao, dgstSHA256, outBase64, True);

  end;

end;

{ TMetodoEnviarEmailCFe }

{ Params: 0 -  cDestinatario: email do destinatário
          1 -  cXMLVenda: String com path do XML de Venda
          2 -  cAssunto: Assunto do Email
          3 -  cMensagem: Mensagem do corpo do e-mail
          4 -  cCC: String com e-mails copia (Separados ;)
          5 -  cAnexos: String com Path de Anexos (Separados ;)
}
procedure TMetodoEnviarEmailCFe.Executar;
var
  cDestinatario: String;
  cXMLVenda: String;
  cAssunto: String;
  cMensagem: String;
  cCC: String;
  cAnexos: String;
  sAssuntoSAT: String;
  slMensagem, slCC, slAnexos: TStringList;

begin
  cDestinatario:= fpCmd.Params(0);
  cXMLVenda:= fpCmd.Params(1);
  cAssunto:= fpCmd.Params(2);
  cMensagem:= fpCmd.Params(3);
  cCC:= fpCmd.Params(4);
  cAnexos:= fpCmd.Params(5);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    ACBrSAT.CFe.Clear;

    slMensagem := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    try
      CarregarDadosVenda(cXMLVenda);
      DoPrepararImpressaoSAT('',False);

      with MonitorConfig.SAT.SATEmail do
      begin
         slMensagem.Text := IfThen(NaoEstaVazio(cMensagem), cMensagem, StringToBinaryString(MensagemSAT));
         sAssuntoSAT := IfThen(NaoEstaVazio(cAssunto),cAssunto, AssuntoSAT);
      end;

      QuebrarLinha(cCC, slCC);
      QuebrarLinha(cAnexos, slAnexos);

      try
        ACBrSAT.EnviarEmail(cDestinatario,
                            sAssuntoSAT,
                            slMensagem,
                            slCC,
                            slAnexos);
        if not(MonitorConfig.Email.SegundoPlano) then
          fpCmd.Resposta := 'E-mail enviado com sucesso!'
        else
          fpCmd.Resposta := 'Enviando e-mail em segundo plano...';

      except
        on E: Exception do
          raise Exception.Create('Erro ao enviar email' + sLineBreak + E.Message);
      end;

    finally
      slMensagem.Free;
      slCC.Free;
      slAnexos.Free;
    end;
  end;
end;

{ TMetodoSetLogoMarca }

{ Params: 0 - cpath: path do logo
}
procedure TMetodoSetLogoMarca.Executar;
var
  cPath : String;
begin
  cPath := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    if FileExists(cPath) then
    begin
      TACBrSATExtratoFortes(ACBrSAT.Extrato).LogoVisible := True;
      TACBrSATExtratoFortes(ACBrSAT.Extrato).PictureLogo.LoadFromFile(cPath);
      with MonitorConfig.DFE do
        Impressao.Geral.LogoMarcaNFCeSAT:= cPath;
      MonitorConfig.SalvarArquivo;

    end
    else
      raise Exception.Create('Arquivo não encontrado.');
  end;

end;

{ TMetodoSetNumeroSessao }

{ Params: 0 - cNumero: Numero de sessão
}
procedure TMetodoSetNumeroSessao.Executar;
var
  cNumero : String;
begin
  cNumero := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
    ACBrSAT.Tag := StrToIntDef(Trim(cNumero), 0);

end;

{ TMetodoTesteFimaFim }

{ Params: 0 - cXMLVenda: String com caminho oo Arquivo XML de Venda
}
procedure TMetodoTesteFimaFim.Executar;
var
  cXMLVenda : String;
  Resultado : String;
begin
  cXMLVenda := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    ACBrSAT.InicializaCFe;
    CarregarDadosVenda(cXMLVenda);
    Resultado := ACBrSAT.TesteFimAFim(ACBrSAT.CFe.GerarXML(True));

    if EstaVazio(Resultado) or (ACBrSat.Resposta.codigoDeRetorno = 0) then
      raise Exception.Create('Nenhuma Resposta de Retorno! ' + sLineBreak
      + 'CodigoDeRetorno: ' + IntToStr(ACBrSat.Resposta.codigoDeRetorno) + ' / Resultado: ' + Resultado);

    RespostaTesteFimaFim(Resultado);

  end;

end;

{ TMetodoExtrairLog }

{ Params: 0 - cPathArq: String com caminho do Arquivo
}
procedure TMetodoExtrairLog.Executar;
var
  cPathArq : String;
begin
  cPathArq := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
    ACBrSAT.ExtrairLogs(cPathArq);

end;

{ TMetodoGerarPDFExtratoVenda }

{ Params: 0 - cXMLVenda: String com XML de Vendas o path do arquivo
          1 - cNomeArq: String com nome do o Arquivo
}
procedure TMetodoGerarPDFExtratoVenda.Executar;
var
  cXMLVenda : String;
  cNomeArq : String;
begin
  cXMLVenda := fpCmd.Params(0);
  cNomeArq := fpCmd.Params(1);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    DoPrepararImpressaoSAT('', True);
    CarregarDadosVenda(cXMLVenda, cNomeArq);
    ACBrSAT.ImprimirExtrato;

    RespostaPadrao;

  end;

end;

{ TMetodoGerarImpressaoFiscalMFe }

{ Params: 0 - cXMLVenda: String com XML de Vendas o path do arquivo
          1 - cImpressora: String com nome da impressora
}
procedure TMetodoGerarImpressaoFiscalMFe.Executar;
var
  cXMLVenda : String;
  cImpressora : String;
begin
  (*cXMLVenda := fpCmd.Params(0);
  cImpressora := fpCmd.Params(1);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    if MonitorConfig.SAT.SATImpressao.SATFortes.UsarFortes then
      raise Exception.Create( 'Falha ao gerar Impressão! Para Geração de Cupom MFe é preciso configurar Impressão ESCPOS. ');

    DoPrepararImpressaoSAT(cImpressora);
    CarregarDadosVenda(cXMLVenda);
    fpCmd.Resposta := TACBrSATExtratoESCPOS(ACBrSAT.Extrato).GerarImpressaoFiscalMFe();

  end;*)

  cXMLVenda := fpCmd.Params(0);
  cImpressora := fpCmd.Params(1);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    DoPrepararImpressaoSAT(cImpressora);
    CarregarDadosVenda(cXMLVenda);
    ACBrSAT.ImprimirExtrato;
  end;
end;


{ TMetodoImprimirExtratoCancelamento }

{ Params: 0 - cXMLVenda: String com XML de Vendas o path do arquivo
          1 - cXMLCancelamento: String com XML de Cancelamentoo path do arquivo
          2 - cImpressora: String com nome da impressora
}
procedure TMetodoImprimirExtratoCancelamento.Executar;
var
  cXMLVenda : String;
  cXMLCancelamento : String;
  cImpressora : String;
begin
  cXMLVenda := fpCmd.Params(0);
  cXMLCancelamento := fpCmd.Params(1);
  cImpressora := fpCmd.Params(2);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    DoPrepararImpressaoSAT(cImpressora);
    CarregarDadosVenda(cXMLVenda);
    CarregarDadosCancelamento(cXMLCancelamento);
    ACBrSAT.ImprimirExtratoCancelamento;
  end;

end;


{ TMetodoImprimirExtratoResumido }

{ Params: 0 - cXMLVenda: String com XML de Vendas o path do arquivo
          1 - cImpressora: String com nome da impressora
}
procedure TMetodoImprimirExtratoResumido.Executar;
var
  cXMLVenda : String;
  cImpressora : String;
begin
  cXMLVenda := fpCmd.Params(0);
  cImpressora := fpCmd.Params(1);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    DoPrepararImpressaoSAT(cImpressora);
    CarregarDadosVenda(cXMLVenda);
    ACBrSAT.ImprimirExtratoResumido;
  end;

end;

{ TMetodoImprimirExtratoVenda }

{ Params: 0 - cXMLVenda: String com XML de Vendas o path do arquivo
          1 - cImpressora: String com nome da impressora
}
procedure TMetodoImprimirExtratoVenda.Executar;
var
  cXMLVenda : String;
  cImpressora : String;
begin
  cXMLVenda := fpCmd.Params(0);
  cImpressora := fpCmd.Params(1);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    DoPrepararImpressaoSAT(cImpressora);
    CarregarDadosVenda(cXMLVenda);
    ACBrSAT.ImprimirExtrato;
  end;

end;

{ TMetodoCancelarCFe }

{ Params: 0 - cArqXMLVenda: String com XML de Venda
}
procedure TMetodoCancelarCFe.Executar;
var
  cArqXMLVenda, Resultado : String;
begin
  cArqXMLVenda := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    if NaoEstaVazio(cArqXMLVenda) then
      CarregarDadosVenda(cArqXMLVenda);

    Resultado := ACBrSAT.CancelarUltimaVenda;

    if EstaVazio(Resultado) or (ACBrSat.Resposta.codigoDeRetorno = 0) then
      raise Exception.Create('Nenhuma Resposta de Retorno! ' + sLineBreak
      + 'CodigoDeRetorno: ' + IntToStr(ACBrSat.Resposta.codigoDeRetorno) + ' / Resultado: ' + Resultado);

    RespostaCancelarVenda(Resultado);

  end;

end;

{ TMetodoEnviarCFe }

{ Params: 0 - cArqXML: String com XML ou Path do arquivo
}
procedure TMetodoEnviarCFe.Executar;
var
  cArqXML, Resultado: String;
  SL: TStringList;
begin
  cArqXML := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    if NaoEstaVazio(cArqXML) then
    begin
      ACBrSAT.CFe.Clear;
      if (pos(#10,cArqXML) = 0) and FileExists(cArqXML) then
      begin
        SL:= TStringList.Create;
        try
          SL.LoadFromFile(cArqXML);
          if not(StringIsXML( SL.Text )) then
            raise Exception.Create('Falha ao carregar o arquivo '+cArqXML+'. XML inválido! ');
          Resultado := ACBrSAT.EnviarDadosVenda(SL.Text);
        finally
          SL.Free;
        end;
      end
      else
      if StringIsXML( cArqXML ) then
        Resultado := ACBrSAT.EnviarDadosVenda( cArqXML )
      else
        raise Exception.Create('Diretório ou XML: '+cArqXML+' inválido! ');

    end
    else if (ACBrSAT.CFe.ide.signAC <> '') then
      Resultado := ACBrSAT.EnviarDadosVenda
    else
      raise Exception.Create('Nenhum XML encontrado para envio! ');

    if EstaVazio(Resultado) or (ACBrSat.Resposta.codigoDeRetorno = 0) then
      raise Exception.Create('Nenhuma Resposta de Retorno! ' + sLineBreak
      + 'CodigoDeRetorno: ' + IntToStr(ACBrSat.Resposta.codigoDeRetorno) + ' / Resultado: ' + Resultado);

    RespostaEnviarDadosVenda( Resultado );

  end;

end;

{ TMetodoCriarEnviarCFe }

{ Params: 0 - cArqIni: String com INI ou Path do arquivo
}
procedure TMetodoCriarEnviarCFe.Executar;
var
  cArqIni, Resultado: String;
begin
  cArqIni := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    GerarIniCFe( cArqIni );
    ACBrSAT.CFe.GerarXML( True ); // Tags da Aplicação

    Resultado := ACBrSAT.EnviarDadosVenda( ACBrSAT.CFe.AsXMLString );

    if EstaVazio(Resultado) or (ACBrSat.Resposta.codigoDeRetorno = 0) then
      raise Exception.Create('Nenhuma Resposta de Retorno! ' + sLineBreak
      + 'CodigoDeRetorno: ' + IntToStr(ACBrSat.Resposta.codigoDeRetorno) + ' / Resultado: ' + Resultado);

    RespostaEnviarDadosVenda( Resultado );

  end;

end;

{ TMetodoCriarCFe }

{ Params: 0 - cArqIni: String com INI ou Path do arquivo
}
procedure TMetodoCriarCFe.Executar;
var
  cArqIni, ArqCFe: String;
begin
  cArqIni := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    GerarIniCFe( cArqIni);

    ArqCFe := '';
    ACBrSAT.CFe.GerarXML( True ); // Tags da Aplicação
    if MonitorConfig.SAT.SalvarCFe then
    begin
      ArqCFe := ACBrSAT.CalcCFeNomeArq(ACBrSAT.ConfigArquivos.PastaEnvio,
                          IntToStrZero(ACBrSAT.CFe.ide.numeroCaixa,3)+'-'+
                          IntToStrZero(ACBrSAT.CFe.ide.cNF,6),'-satcfe');
      ACBrSAT.CFe.SaveToFile(ArqCFe);
    end;

    RespostaCriarCFe(ArqCFe);

  end;

end;

{ TMetodoCarregarDadosCancelamento }

{ Params: 0 - aStr: Path do arquivo
}
procedure TMetodoCarregarDadosCancelamento.Executar;
var
  aStr : String;
begin
  aStr := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
    CarregarDadosCancelamento(aStr);

end;

{ TMetodoCarregarDadosVenda }

{ Params: 0 - aStr: Path do arquivo
}
procedure TMetodoCarregarDadosVenda.Executar;
var
  aStr : String;
begin
  aStr := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
   CarregarDadosVenda(aStr);

end;

{ TMetodoComunicarCertificado }

{ Params: 0 - cNumeroCertificado
}
procedure TMetodoComunicarCertificado.Executar;
var
  cNumeroCertificado: AnsiString;
begin
  cNumeroCertificado     :=  fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrSAT.ComunicarCertificadoICPBRASIL(cNumeroCertificado);
  end;

end;

{ TMetodoAtualizaSoftware }

procedure TMetodoAtualizaSoftware.Executar;
begin
  with TACBrObjetoSAT(fpObjetoDono) do
    fpCmd.Resposta := ACBrSAT.AtualizarSoftwareSAT;


end;

{ TMetodoConsultarSessao }

{ Params: 0 - cConsultarSessao
}
procedure TMetodoConsultarSessao.Executar;
var
  cConsultarSessao: String;
begin
  cConsultarSessao     :=  fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    ACBrSAT.CFe.Clear;
    ACBrSAT.CFeCanc.Clear;

    fpCmd.Resposta := ACBrSAT.ConsultarNumeroSessao(StrToInt(cConsultarSessao));

    if ACBrSAT.Resposta.codigoDeRetorno = 6000 then
      RespostaConsultaSessao(ACBrSAT.CFe.NomeArquivo);

    if ACBrSAT.Resposta.codigoDeRetorno = 7000 then
      RespostaConsultaSessaoCancelado(ACBrSAT.CFeCanc.NomeArquivo);

  end;

end;

{ TMetodoConsultarStatusOperacional }

procedure TMetodoConsultarStatusOperacional.Executar;
begin
  with TACBrObjetoSAT(fpObjetoDono) do
  begin
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_ConsultarUltimaSessaoFiscal>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ConsultarUltimaSessaoFiscalResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }
    fpCmd.Resposta := ACBrSAT.ConsultarStatusOperacional;

    if (ACBrSAT.Resposta.codigoDeRetorno = 10000) then
      RespostaStatusSAT;

  end;

end;

{ TMetodoConsultarSat }

procedure TMetodoConsultarSat.Executar;
begin
  with TACBrObjetoSAT(fpObjetoDono) do
    fpCmd.Resposta := ACBrSAT.ConsultarSAT;

end;

{ TMetodoConsultarUltimaSessaoFiscal }

procedure TMetodoConsultarUltimaSessaoFiscal.Executar;
begin
  with TACBrObjetoSAT(fpObjetoDono) do
    fpCmd.Resposta := ACBrSAT.ConsultarUltimaSessaoFiscal;

end;


{ TMetodoTrocarCodigoAtivacao }

{ Params: 0 - cCodAtivacao - Uma String com código para ativação
          1 - cOpcao - Uma String com a assinatura
          2 - cNovoCodAtivacao - Uma String com novo código para ativação
}
procedure TMetodoTrocarCodigoAtivacao.Executar;
var
cCodAtivacao, cOpcao, cNovoCodAtivacao: String;
begin
  cCodAtivacao     :=  fpCmd.Params(0);
  cOpcao           :=  fpCmd.Params(1);
  cNovoCodAtivacao :=  fpCmd.Params(2);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrSAT.TrocarCodigoDeAtivacao(cCodAtivacao,
                               StrToIntDef(cOpcao,1), cNovoCodAtivacao)
  end;

end;

{ TMetodoDesbloquear }

procedure TMetodoDesbloquear.Executar;
begin
  with TACBrObjetoSAT(fpObjetoDono) do
    fpCmd.Resposta := ACBrSAT.DesbloquearSAT;
end;

{ TMetodoBloquear }

{ Params:
}
procedure TMetodoBloquear.Executar;
begin
  with TACBrObjetoSAT(fpObjetoDono) do
    fpCmd.Resposta := ACBrSAT.BloquearSAT;
end;

{ TMetodoAssociarAssinatura }

{ Params: 0 - CNPJs - Uma String contendo o CNPJ da Sw.House + CNPJ do Emissor para ativação
          1 - Assinatura - Uma String com a assinatura
}
procedure TMetodoAssociarAssinatura.Executar;
var
  cCNPJs, cCNPJSwHouse, cCNPJEmissor, cAssinatura: String;
begin
  cCNPJs := fpCmd.Params(0);
  cCNPJSwHouse := copy(cCNPJs, 1,14);
  cCNPJEmissor := copy(cCNPJs,15,14);
  cAssinatura := fpCmd.Params(1);

  with TACBrObjetoSAT(fpObjetoDono), MonitorConfig do
  begin
    if EstaVazio(Trim(cCNPJSwHouse)) then
      cCNPJSwHouse := SAT.SATSWH.CNPJ;

    if EstaVazio(Trim(cCNPJEmissor)) then
      cCNPJEmissor := SAT.SATImpressao.SATEmit.CNPJ;

    if EstaVazio(Trim(cAssinatura)) then
      cAssinatura := SAT.SATSWH.Assinatura;

    if (ACBrSAT.Config.ide_tpAmb <> taHomologacao) then
    begin
       if (not ValidarCNPJ(cCNPJSwHouse)) then
         raise Exception.Create('CNPJ Sw.House inválido: '+cCNPJSwHouse);

       if (not ValidarCNPJ(cCNPJEmissor)) then
         raise Exception.Create('CNPJ Emissor inválido: '+cCNPJEmissor);
    end;

    fpCmd.Resposta := ACBrSAT.AssociarAssinatura(cCNPJSwHouse + cCNPJEmissor, cAssinatura);
  end;
end;

{ TMetodoDesinicializar }

procedure TMetodoDesinicializar.Executar;
begin
  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    if not ACBrSAT.Inicializado then
      fpCmd.Resposta := 'SAT não inicializado'
    else
    begin
      ACBrSAT.DesInicializar;
      fpCmd.Resposta := 'SAT desinicializado'
    end;
  end;

end;

{ TMetodoInicializar }

{ Params:
}
procedure TMetodoInicializar.Executar;
begin
  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    if ACBrSAT.Inicializado then
       fpCmd.Resposta := 'SAT ja inicializado'
    else
    begin
       ACBrSAT.Inicializar;
       fpCmd.Resposta := 'SAT inicializado';
    end;
  end;

end;

{ TMetodoAtivar }


{ Params: 0 - CNPJ - Uma String com CNPJ para ativação
          1 - UF - Uma String com código da UF para ativação
}
procedure TMetodoAtivar.Executar;
var
  cCNPJ, cUF: String;
begin
  cCNPJ := fpCmd.Params(0);
  cUF :=   fpCmd.Params(1);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    if (EstaVazio(Trim(cCNPJ)) and
          EstaVazio(Trim(cUF))) then
        with MonitorConfig.SAT do
          fpCmd.Resposta := ACBrSAT.AtivarSAT(1, OnlyNumber(SATImpressao.SATEmit.CNPJ), StrToInt(CodigoUF))
      else
      begin
        if (ACBrSAT.Config.ide_tpAmb <> taHomologacao) and
           (not ValidarCNPJ(cCNPJ)) then
          raise Exception.Create('CNPJ '+cCNPJ+' inválido.');

        fpCmd.Resposta := ACBrStr( ACBrSAT.AtivarSAT(1,cCNPJ, StrToInt(cUF)) );
      end;

    end;

end;

{ TACBrObjetoSAT }

constructor TACBrObjetoSAT.Create(AConfig: TMonitorConfig; ACBrSAT: TACBrSAT);
begin
  inherited Create(AConfig);

  fACBrSAT := ACBrSAT;

  ListaDeMetodos.Add(CMetodoSATAtivar);
  ListaDeMetodos.Add(CMetodoInicializar);
  ListaDeMetodos.Add(CMetodoDesInicializar);
  ListaDeMetodos.Add(CMetodoAssociarAssinatura);
  ListaDeMetodos.Add(CMetodoBloquear);
  ListaDeMetodos.Add(CMetodoDesbloquear);
  ListaDeMetodos.Add(CMetodotrocarcodigoativacao);
  ListaDeMetodos.Add(CMetodoConsultarSat);
  ListaDeMetodos.Add(CMetodoConsultarStatusOperacional);
  ListaDeMetodos.Add(CMetodoConsultarSessao);
  ListaDeMetodos.Add(CMetodoConsultarNumeroSessao);
  ListaDeMetodos.Add(CMetodoAtualizaSoftware);
  ListaDeMetodos.Add(CMetodoAtualizarSoftwareSAT);
  ListaDeMetodos.Add(CMetodoComunicarCertificado);
  ListaDeMetodos.Add(CMetodoComunicarCertificadoICPBrasil);
  ListaDeMetodos.Add(CMetodoCarregarDadosVenda);
  ListaDeMetodos.Add(CMetodoCarregarDadosCancelamento);
  ListaDeMetodos.Add(CMetodoCriarCFe);
  ListaDeMetodos.Add(CMetodoCriarEnviarCFe);
  ListaDeMetodos.Add(CMetodoEnviarCFe);
  ListaDeMetodos.Add(CMetodoCancelarCFe);
  ListaDeMetodos.Add(CMetodoImprimirExtratoVenda);
  ListaDeMetodos.Add(CMetodoImprimirExtratoResumido);
  ListaDeMetodos.Add(CMetodoImprimirExtratoCancelamento);
  ListaDeMetodos.Add(CMetodoGerarImpressaoFiscalMFe);
  ListaDeMetodos.Add(CMetodoExtrairLogs);
  ListaDeMetodos.Add(CMetodoTesteFimaFim);
  ListaDeMetodos.Add(CMetodoGerarPDFExtratoVenda);
  ListaDeMetodos.Add(CMetodoSetNumeroSessao);
  ListaDeMetodos.Add(CMetodoSetlogomarcaSAT);
  ListaDeMetodos.Add(CMetodoGerarAssinaturaSAT);
  ListaDeMetodos.Add(CMetodoEnviarEmailCFe);
  ListaDeMetodos.Add(CMetodoConsultarModeloSAT);
  ListaDeMetodos.Add(CMetodoGerarPDFExtratoCancelamento);
  ListaDeMetodos.Add(CMetodoConsultarUltimaSessaoFiscal);


  // DoACBr
  ListaDeMetodos.Add(CMetodoSavetofile);
  ListaDeMetodos.Add(CMetodoLoadfromfile);
  ListaDeMetodos.Add(CMetodoLerini);
  ListaDeMetodos.Add(CMetodoSetcertificado);
  ListaDeMetodos.Add(CMetodoRestaurar);
  ListaDeMetodos.Add(CMetodoOcultar);
  ListaDeMetodos.Add(CMetodoEncerrarmonitor);
  ListaDeMetodos.Add(CMetodoAtivo);
  ListaDeMetodos.Add(CMetodoDatahora);
  ListaDeMetodos.Add(CMetodoData);
  ListaDeMetodos.Add(CMetodoHora);
  ListaDeMetodos.Add(CMetodoExit);
  ListaDeMetodos.Add(CMetodoBye);
  ListaDeMetodos.Add(CMetodoFim);
  ListaDeMetodos.Add(CMetodoSair);

end;

procedure TACBrObjetoSAT.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
  AACBrUnit: TACBrObjetoACBr;
begin
  inherited Executar(ACmd);

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoAtivar;
    1  : AMetodoClass := TMetodoInicializar;
    2  : AMetodoClass := TMetodoDesinicializar;
    3  : AMetodoClass := TMetodoAssociarAssinatura;
    4  : AMetodoClass := TMetodoBloquear;
    5  : AMetodoClass := TMetodoDesbloquear;
    6  : AMetodoClass := TMetodoTrocarCodigoAtivacao;
    7  : AMetodoClass := TMetodoConsultarSat;
    8  : AMetodoClass := TMetodoConsultarStatusOperacional;
    9  : AMetodoClass := TMetodoConsultarSessao;
    10 : AMetodoClass := TMetodoConsultarSessao;
    11 : AMetodoClass := TMetodoAtualizaSoftware;
    12 : AMetodoClass := TMetodoAtualizaSoftware;
    13 : AMetodoClass := TMetodoComunicarCertificado;
    14 : AMetodoClass := TMetodoComunicarCertificado;
    15 : AMetodoClass := TMetodoCarregarDadosVenda;
    16 : AMetodoClass := TMetodoCarregarDadosCancelamento;
    17 : AMetodoClass := TMetodoCriarCFe;
    18 : AMetodoClass := TMetodoCriarEnviarCFe;
    19 : AMetodoClass := TMetodoEnviarCFe;
    20 : AMetodoClass := TMetodoCancelarCFe;
    21 : AMetodoClass := TMetodoImprimirExtratoVenda;
    22 : AMetodoClass := TMetodoImprimirExtratoResumido;
    23 : AMetodoClass := TMetodoImprimirExtratoCancelamento;
    24 : AMetodoClass := TMetodoGerarImpressaoFiscalMFe;
    25 : AMetodoClass := TMetodoExtrairLog;
    26 : AMetodoClass := TMetodoTesteFimaFim;
    27 : AMetodoClass := TMetodoGerarPDFExtratoVenda;
    28 : AMetodoClass := TMetodoSetNumeroSessao;
    29 : AMetodoClass := TMetodoSetLogoMarca;
    30 : AMetodoClass := TMetodoGerarAssinaturaSAT;
    31 : AMetodoClass := TMetodoEnviarEmailCFe;
    32 : AMetodoClass := TMetodoConsultarModeloSAT;
    33 : AMetodoClass := TMetodoGerarPDFExtratoCancelamento;
    34 : AMetodoClass := TMetodoConsultarUltimaSessaoFiscal;


    else
      begin
        AACBrUnit := TACBrObjetoACBr.Create(Nil); //Instancia DoACBrUnit para validar métodos padrão para todos os objetos
        try
          AACBrUnit.Executar(ACmd);
        finally
          AACBrUnit.Free;
        end;

      end;

  end;

  if Assigned(AMetodoClass) then
  begin
    Ametodo := AMetodoClass.Create(ACmd, Self);
    try
      Ametodo.Executar;
    finally
      Ametodo.Free;
    end;
  end;

end;

procedure TACBrObjetoSAT.RespostaConsultaSessao(ArqCFe: String);
var
  Resp: TRetornoConsultarSessao;
begin
  Resp := TRetornoConsultarSessao.Create(TpResp, codUTF8);
  try
    with fACBrSAT.CFe do
    begin
      Resp.nCFe := IntToStrZero(ide.nCFe,0);
      Resp.XML  := AsXMLString;
      Resp.Arquivo:= ArqCFe;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
      RespostaIntegrador;
    end;
  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoSAT.RespostaConsultaSessaoCancelado(ArqCFe: String);
var
  Resp: TRetornoConsultarSessaoCancelado;
begin
  Resp := TRetornoConsultarSessaoCancelado.Create(TpResp, codUTF8);
  try
    with fACBrSAT.CFeCanc do
    begin
      Resp.nCFeCanc := IntToStrZero(ide.nCFe,0);
      Resp.XML  := AsXMLString;
      Resp.Arquivo:= ArqCFe;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
      RespostaIntegrador;

    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoSAT.RespostaStatusSAT;
var
  Resp: TRetornoStatusSAT;
begin
  Resp := TRetornoStatusSAT.Create(TpResp, codUTF8);
  try
    with fACBrSAT.Status do
    begin
      Resp.NSERIE :=            NSERIE;
      Resp.LAN_MAC :=           LAN_MAC;
      Resp.STATUS_LAN :=        StatusLanToStr(STATUS_LAN);
      Resp.NIVEL_BATERIA :=     NivelBateriaToStr(NIVEL_BATERIA);
      Resp.MT_TOTAL :=          MT_TOTAL;
      Resp.MT_USADA :=          MT_USADA;
      Resp.DH_ATUAL :=          DH_ATUAL;
      Resp.VER_SB :=            VER_SB;
      Resp.VER_LAYOUT :=        VER_LAYOUT;
      Resp.ULTIMO_CFe :=        ULTIMO_CFe;
      Resp.LISTA_INICIAL :=     LISTA_INICIAL;
      Resp.LISTA_FINAL :=       LISTA_FINAL;
      Resp.DH_CFe :=            DH_CFe;
      Resp.DH_ULTIMA :=         DH_ULTIMA;
      Resp.CERT_EMISSAO :=      CERT_EMISSAO;
      Resp.CERT_VENCIMENTO :=   CERT_VENCIMENTO;
      Resp.ESTADO_OPERACAO :=   EstadoOperacaoToStr(ESTADO_OPERACAO);

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
      RespostaIntegrador;
    end;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoSAT.RespostaCriarCFe(ArqCFe: String);
var
  Resp: TRetornoCriarCFe;
begin
  Resp := TRetornoCriarCFe.Create(TpResp, codUTF8);
  try
    with fACBrSAT.CFe do
    begin
      Resp.nCFe := IntToStr(ide.nCFe);
      Resp.XML  := AsXMLString;
      Resp.Arquivo:= ArqCFe;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
      RespostaIntegrador;
    end;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoSAT.RespostaEnviarDadosVenda(Resultado: String);
var
  ArqCFe: String;
  Resp: TRetornoEnvio;
begin
  Resp := TRetornoEnvio.Create(TpResp, codUTF8);
  try
    with fACBrSAT do
    begin
      ArqCFe := CFe.NomeArquivo;
      Resp.Resultado := Resultado;
      Resp.NumeroSessao  := Resposta.numeroSessao;
      Resp.CodigoDeRetorno := Resposta.codigoDeRetorno;
      Resp.RetornoStr := Resposta.RetornoStr;
      if (ArqCFe <> '') and FileExists(ArqCFe) then
        Resp.Arquivo := ArqCFe;
      Resp.XML := CFe.AsXMLString;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
      RespostaIntegrador;
    end;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoSAT.RespostaCancelarVenda(Resultado: String);
var
  ArqCFe: String;
  Resp: TRetornoCancelarCFe;
begin
  Resp := TRetornoCancelarCFe.Create(TpResp, codUTF8);
  try
    with fACBrSAT do
    begin
      ArqCFe := CFeCanc.NomeArquivo;
      Resp.Resultado := Resultado;
      Resp.NumeroSessao  := Resposta.numeroSessao;
      Resp.CodigoDeRetorno := Resposta.codigoDeRetorno;
      Resp.RetornoStr := Resposta.RetornoStr;
      if (ArqCFe <> '') and FileExists(ArqCFe) then
        Resp.Arquivo := ArqCFe;
      Resp.XML := CFeCanc.AsXMLString;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
      RespostaIntegrador;
    end;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoSAT.RespostaTesteFimaFim(Resultado: String);
var
  Resp: TRetornoTesteFimaFim;
begin
  Resp := TRetornoTesteFimaFim.Create(TpResp, codUTF8);
  try
    with fACBrSAT do
    begin
      Resp.Resultado := Resultado;
      Resp.NumeroSessao  := Resposta.numeroSessao;
      Resp.CodigoDeRetorno := Resposta.codigoDeRetorno;
      Resp.RetornoStr := Resposta.RetornoStr;
      Resp.XML := CFe.AsXMLString;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
      RespostaIntegrador;

    end;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoSAT.RespostaPadrao;
var
  Resp: TPadraoSATResposta;
begin
  Resp := TPadraoSATResposta.Create('CFe',TpResp, codUTF8);
  try
    with fACBrSAT do
    begin
      Resp.Arquivo:= Extrato.NomeDocumento;
      Resp.XML:= Extrato.CFe.XMLOriginal;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;

    end;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoSAT.RespostaPadraoCancelamento;
var
  Resp: TPadraoSATResposta;
begin
  Resp := TPadraoSATResposta.Create('CFe',TpResp, codUTF8);
  try
    with fACBrSAT do
    begin
      Resp.Arquivo:= Extrato.NomeDocumento;
      Resp.XML:= Extrato.CFeCanc.XMLOriginal;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;

    end;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoSAT.RespostaIntegrador;
begin
  with fACBrSAT do
    fpCmd.Resposta := fpCmd.Resposta + DoRespostaIntegrador();

end;


end.

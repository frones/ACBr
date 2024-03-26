{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibSATBase;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibSATDataModule;

type

  { TACBrLibSAT }

  TACBrLibSAT = class(TACBrLib)
  private
    FSatDM: TLibSatDM;

  protected
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;

  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property SatDM: TLibSatDM read FSatDM;

    function InicializarSAT: longint;
    function DesInicializar: longint;
    function AtivarSAT(CNPJvalue: PChar; cUF: longint;
      const sResposta: PChar; var esTamanho: longint):longint;
    function AssociarAssinatura(CNPJvalue, assinaturaCNPJs: PChar;
                                const sResposta: PChar; var esTamanho: longint): longint;
    function BloquearSAT(const sResposta: PChar; var esTamanho: longint): longint;
    function DesbloquearSAT(const sResposta: PChar; var esTamanho: longint): longint;
    function TrocarCodigoDeAtivacao(codigoDeAtivacaoOuEmergencia: PChar; opcao: integer; novoCodigo: PChar;
                                    const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarSAT(const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarUltimaSessaoFiscal(const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarStatusOperacional(const sResposta: PChar;  var esTamanho: longint): longint;
    function ConsultarNumeroSessao(cNumeroDeSessao: integer; const sResposta: PChar; var esTamanho: longint): longint;
    function SetNumeroSessao(cNumeroDeSessao: PChar):longint;
    function AtualizarSoftwareSAT(const sResposta: PChar; var esTamanho: longint): longint;
    function ComunicarCertificadoICPBRASIL(certificado: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function ExtrairLogs(eArquivo: PChar): longint;
    function TesteFimAFim(eArquivoXmlVenda: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function GerarAssinaturaSAT(eCNPJSHW, eCNPJEmitente: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function CriarCFe(eArquivoIni: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function CriarEnviarCFe(eArquivoIni: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function ValidarCFe(eArquivoXml: PChar): longint;
    function EnviarCFe(eArquivoXml: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function CancelarCFe(eArquivoXml: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function ImprimirExtratoVenda(eArqXMLVenda, eNomeImpressora: PChar): longint;
    function ImprimirExtratoResumido(eArqXMLVenda, eNomeImpressora: PChar): longint;
    function ImprimirExtratoCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeImpressora: PChar): longint;
    function GerarImpressaoFiscalMFe(eArqXMLVenda: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function GerarPDFExtratoVenda(eArqXMLVenda, eNomeArquivo: PChar; const sResposta: PChar;
                                  var esTamanho: longint): longint;
    function GerarPDFCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeArquivo: PChar;
                                  const sResposta: PChar; var esTamanho: longint): longint;
    function EnviarEmail(eArqXMLVenda, sPara, sAssunto, eNomeArquivo, sMensagem, sCC, eAnexos: PChar): longint;

  end;

implementation

uses
  ACBrLibConsts, ACBrLibSATConsts, ACBrLibConfig, ACBrLibSATConfig,
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrLibResposta, ACBrLibSATRespostas, ACBrMail, ACBrDFeSSL, ACBrValidador,
  ACBrSATExtratoESCPOS;

{ TACBrLibSAT }

constructor TACBrLibSAT.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  FSatDM := TLibSatDM.Create(nil);
  FSatDM.Lib := Self;
end;

destructor TACBrLibSAT.Destroy;
begin
  FSatDM.Free;
  inherited Destroy;
end;

procedure TACBrLibSAT.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibSATConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibSAT.Executar;
begin
  inherited Executar;
  FSatDM.AplicarConfiguracoes;
end;

function TACBrLibSAT.InicializarSAT: longint;
begin
  try
    GravarLog('SAT_InicializarSAT', logNormal);

    SatDM.Travar;
    try
      if SatDM.ACBrSAT1.Inicializado then
      begin
        Result := SetRetorno(-8, 'SAT já inicializado');
      end
      else
      begin
        SatDM.ACBrSAT1.Inicializar;
        Result := SetRetorno(ErrOK);
      end;
    finally
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.DesInicializar: longint;
begin
  try
    GravarLog('SAT_DesInicializar', logNormal);

    SatDM.Travar;
    try
      if not SatDM.ACBrSAT1.Inicializado then
      begin
        Result := SetRetorno(-8, 'SAT não inicializado');
      end
      else
      begin
        SatDM.ACBrSAT1.DesInicializar;
        Result := SetRetorno(ErrOK);
      end;
    finally
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.AtivarSAT(CNPJvalue: PChar; cUF: longint;
                              const sResposta: PChar; var esTamanho: longint):longint;
var
  CNPJ, Resposta: Ansistring;
  RespSat: TACBrLibSATResposta;
begin
  try
    CNPJ := ConverterAnsiParaUTF8(CNPJvalue);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_AtivarSAT(' + CNPJ + ',' + IntToStr(cUF) + ' )', logCompleto, True)
    else
      GravarLog('SAT_AtivarSAT', logNormal);

    SatDM.Travar;

    RespSat := TACBrLibSATResposta.Create(Config.TipoResposta, Config.CodResposta);
    try
     Resposta := '';
     SatDM.ACBrSAT1.AtivarSAT(1, CNPJ, cUF);
     RespSat.Processar(SatDM.ACBrSAT1);
     Resposta := RespSat.Gerar;

     MoverStringParaPChar(Resposta, sResposta, esTamanho);
     Result := SetRetorno(ErrOK, Resposta);
    finally
      RespSat.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.AssociarAssinatura(CNPJvalue, assinaturaCNPJs: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
var
  CNPJ, Assinatura, Resposta: ansistring;
  RespSat: TACBrLibSATResposta;
begin
  try
    CNPJ := ConverterAnsiParaUTF8(CNPJvalue);
    Assinatura := ConverterAnsiParaUTF8(assinaturaCNPJs);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_AssociarAssinatura(' + CNPJ + ',' + Assinatura + ' )', logCompleto, True)
    else
      GravarLog('SAT_AssociarAssinatura', logNormal);

    SatDM.Travar;

    RespSat := TACBrLibSATResposta.Create(Config.TipoResposta, Config.CodResposta);
    try
     Resposta := '';
     SatDM.ACBrSAT1.AssociarAssinatura(CNPJ, Assinatura);
     RespSat.Processar(SatDM.ACBrSAT1);
     Resposta := RespSat.Gerar;

     MoverStringParaPChar(Resposta, sResposta, esTamanho);
     Result := SetRetorno(ErrOK, Resposta);
    finally
      RespSat.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.BloquearSAT(const sResposta: PChar; var esTamanho: longint): longint;
var
  Resposta: ansistring;
  RespSat: TACBrLibSATResposta;
begin
  try
    GravarLog('SAT_BloquearSAT', logNormal);

    SatDM.Travar;
    RespSat := TACBrLibSATResposta.Create(Config.TipoResposta, Config.CodResposta);
    try
     Resposta := '';
     SatDM.ACBrSAT1.BloquearSAT;
     RespSat.Processar(SatDM.ACBrSAT1);
     Resposta := RespSat.Gerar;
     MoverStringParaPChar(Resposta, sResposta, esTamanho);
     Result := SetRetorno(ErrOK, Resposta);
    finally
      RespSat.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.DesbloquearSAT(const sResposta: PChar; var esTamanho: longint): longint;
var
  Resposta: ansistring;
  RespSat: TACBrLibSATResposta;
begin
  try
    GravarLog('SAT_DesbloquearSAT', logNormal);

    SatDM.Travar;
    RespSat := TACBrLibSATResposta.Create(Config.TipoResposta, Config.CodResposta);
    try
     Resposta := '';
     SatDM.ACBrSAT1.DesbloquearSAT;
     RespSat.Processar(SatDM.ACBrSAT1);
     Resposta := RespSat.Gerar;
     MoverStringParaPChar(Resposta, sResposta, esTamanho);
     Result := SetRetorno(ErrOK, Resposta);
    finally
      RespSat.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.TrocarCodigoDeAtivacao(codigoDeAtivacaoOuEmergencia: PChar;
  opcao: integer; novoCodigo: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  CodigoAtivacao, NovoCodigoAtv, Resposta: ansistring;
  RespSat: TACBrLibSATResposta;
begin
  try
    CodigoAtivacao := ConverterAnsiParaUTF8(codigoDeAtivacaoOuEmergencia);
    NovoCodigoAtv := ConverterAnsiParaUTF8(novoCodigo);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_TrocarCodigoDeAtivacao(' + CodigoAtivacao + ',' + IntToStr(opcao) + ',' + NovoCodigoAtv +
                ' )', logCompleto, True)
    else
      GravarLog('SAT_TrocarCodigoDeAtivacao', logNormal);

    SatDM.Travar;

    RespSat := TACBrLibSATResposta.Create(Config.TipoResposta, Config.CodResposta);
    try
      Resposta := '';
      SatDM.ACBrSAT1.TrocarCodigoDeAtivacao(CodigoAtivacao, opcao, NovoCodigoAtv);
      RespSat.Processar(SatDM.ACBrSAT1);
      Resposta := RespSat.Gerar;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      RespSat.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.ConsultarSAT(const sResposta: PChar; var esTamanho: longint): longint;
var
  Resposta: ansistring;
  RespSat: TACBrLibSATResposta;
begin
  try
    GravarLog('SAT_ConsultarSAT', logNormal);

    SatDM.Travar;

    RespSat := TACBrLibSATResposta.Create(Config.TipoResposta, Config.CodResposta);

    try
      Resposta := '';
      SatDM.ACBrSAT1.ConsultarSAT;
      RespSat.Processar(SatDM.ACBrSAT1);
      Resposta := RespSat.Gerar;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      RespSat.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.ConsultarUltimaSessaoFiscal(const sResposta: PChar; var esTamanho: longint): longint;
var
  Resposta: ansistring;
  RespSat: TACBrLibSATResposta;
begin
  try
    GravarLog('SAT_ConsultarUltimaSessaoFiscal', logNormal);

    SatDM.Travar;

    RespSat := TACBrLibSATResposta.Create(Config.TipoResposta, Config.CodResposta);

    try
      Resposta := '';
      SatDM.ACBrSAT1.ConsultarUltimaSessaoFiscal;
      RespSat.Processar(SatDM.ACBrSAT1);
      Resposta := RespSat.Gerar;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      RespSat.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.ConsultarStatusOperacional(const sResposta: PChar; var esTamanho: longint): longint;
var
  Resposta: ansistring;
  RespSat: TACBrLibSATResposta;
  Resp: TRetornoStatusSAT;
begin
  try
    GravarLog('SAT_ConsultarStatusOperacional', logNormal);

    SatDM.Travar;

    RespSat := TACBrLibSATResposta.Create(Config.TipoResposta, Config.CodResposta);
    Resp := TRetornoStatusSAT.Create(Config.TipoResposta, Config.CodResposta);
    try
      Resposta := '';
      SatDM.ACBrSAT1.ConsultarStatusOperacional;
      RespSat.Processar(SatDM.ACBrSAT1);
      Resposta := RespSat.Gerar;

      if (SatDM.ACBrSAT1.Resposta.codigoDeRetorno = 10000) then
      begin
        Resp.Processar(SatDM.ACBrSAT1);
        Resposta := Resposta + sLineBreak + Resp.Gerar;
      end;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      RespSat.Free;
      Resp.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.ConsultarNumeroSessao(cNumeroDeSessao: integer; const sResposta: PChar;
                                           var esTamanho: longint): longint;
var
  Resposta: ansistring;
  RespSat: TACBrLibSATResposta;
  Resp: TRetornoConsultarSessao;
  RespCanc: TRetornoConsultarSessaoCancelado;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_ConsultarNumeroSessao(' + IntToStr(cNumeroDeSessao) + ' )', logCompleto, True)
    else
      GravarLog('SAT_ConsultarNumeroSessao', logNormal);

    SatDM.Travar;
    RespSat := TACBrLibSATResposta.Create(Config.TipoResposta, Config.CodResposta);

    try
      Resposta := '';
      SatDM.ACBrSAT1.CFe.Clear;
      SatDM.ACBrSAT1.CFeCanc.Clear;

      SatDM.ACBrSAT1.ConsultarNumeroSessao(cNumeroDeSessao);
      RespSat.Processar(SatDM.ACBrSAT1);
      Resposta := RespSat.Gerar;

      if SatDM.ACBrSAT1.Resposta.codigoDeRetorno = 6000 then
      begin
        Resp := TRetornoConsultarSessao.Create(Config.TipoResposta, Config.CodResposta);
        try
          Resp.Processar(SatDM.ACBrSAT1);
          Resposta := Resposta + sLineBreak + Resp.Gerar;
        finally
          Resp.Free;
        end;
      end;

      if SatDM.ACBrSAT1.Resposta.codigoDeRetorno = 7000 then
      begin
        RespCanc := TRetornoConsultarSessaoCancelado.Create(Config.TipoResposta, Config.CodResposta);
        try
          RespCanc.Processar(SatDM.ACBrSAT1);
          Resposta := Resposta + sLineBreak + RespCanc.Gerar;
        finally
          RespCanc.Free;
        end;
      end;
     
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      RespSat.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.SetNumeroSessao(cNumeroDeSessao: PChar):longint;
var
  Sessao: String;
begin
  try
    Sessao:= ConverterAnsiParaUTF8(cNumeroDeSessao);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_SetNumeroSessao(' + cNumeroDeSessao + ' )', logCompleto, True)
    else
      GravarLog('SAT_SetNumeroSessao', logNormal);

    SatDM.Travar;
    try
      SatDM.ACBrSAT1.Tag:= StrToIntDef(Trim(cNumeroDeSessao),0);
      Result := SetRetorno(ErrOK);
    finally
      SatDM.Destravar;
    end;

  except
    on E: EACBrLibException do
    Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
    Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.AtualizarSoftwareSAT(const sResposta: PChar; var esTamanho: longint): longint;
var
  Resposta: ansistring;
  RespSat: TACBrLibSATResposta;
begin
  try
    GravarLog('SAT_AtualizarSoftwareSAT', logNormal);

    SatDM.Travar;
    RespSat := TACBrLibSATResposta.Create(Config.TipoResposta, Config.CodResposta);

    try
      Resposta := '';
      SatDM.ACBrSAT1.AtualizarSoftwareSAT;
      RespSat.Processar(SatDM.ACBrSAT1);
      Resposta := RespSat.Gerar;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      RespSat.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.ComunicarCertificadoICPBRASIL(certificado: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
var
  cCertificado, Resposta: ansistring;
  RespSat: TACBrLibSATResposta;
begin
  try
    cCertificado := ConverterAnsiParaUTF8(certificado);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_ComunicarCertificadoICPBRASIL(' + cCertificado + ' )', logCompleto, True)
    else
      GravarLog('SAT_ComunicarCertificadoICPBRASIL', logNormal);


    SatDM.Travar;
    RespSat := TACBrLibSATResposta.Create(Config.TipoResposta, Config.CodResposta);
    try
      Resposta := '';
      SatDM.ACBrSAT1.ComunicarCertificadoICPBRASIL(cCertificado);
      RespSat.Processar(SatDM.ACBrSAT1);
      Resposta := RespSat.Gerar;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      RespSat.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.ExtrairLogs(eArquivo: PChar): longint;
var
  cArquivo: ansistring;
begin
  try
    cArquivo := ConverterAnsiParaUTF8(eArquivo);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_ExtrairLogs(' + cArquivo + ' )', logCompleto, True)
    else
      GravarLog('SAT_ExtrairLogs', logNormal);

    SatDM.Travar;
    try
      SatDM.ACBrSAT1.ExtrairLogs(cArquivo);
      Result := SetRetorno(ErrOK);
    finally
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.TesteFimAFim(eArquivoXmlVenda: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
var
  ArquivoXmlVenda: ansistring;
  Resposta: String;
  Resp: TRetornoTesteFimaFim;
begin
  try
    ArquivoXmlVenda := ConverterAnsiParaUTF8(eArquivoXmlVenda);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_TesteFimAFim(' + ArquivoXmlVenda + ' )', logCompleto, True)
    else
      GravarLog('SAT_TesteFimAFim', logNormal);

    SatDM.Travar;
    Resp := TRetornoTesteFimaFim.Create(Config.TipoResposta, Config.CodResposta);
    try
      Resposta := '';
      Resp.Resultado := SatDM.ACBrSAT1.TesteFimAFim(ArquivoXmlVenda);
      Resp.Processar(SatDM.ACBrSAT1);
      Resposta := Resp.Gerar;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      Resp.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.GerarAssinaturaSAT(eCNPJSHW, eCNPJEmitente: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
Var
  cCNPJShw, cCNPJEmitente, cCodigoVinculacao, Resposta: String;
begin
  try
    cCNPJShw := OnlyNumber(ConverterAnsiParaUTF8(eCNPJSHW));
    cCNPJEmitente := OnlyNumber(ConverterAnsiParaUTF8(eCNPJEmitente));

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_GerarAssinaturaSAT(' + cCNPJShw + ', ' + cCNPJEmitente + ' )', logCompleto, True)
    else
      GravarLog('SAT_GerarAssinaturaSAT', logNormal);

    SatDM.Travar;

    try
      Resposta := '';

      Resposta := Trim(ACBrValidador.ValidarCNPJ(cCNPJShw));
      if NaoEstaVazio(Resposta) then
        raise EACBrLibException.Create(ErrCNPJInvalido, Format(SErrLibSATCNPJSwHouseInvalido, [Resposta]));

      Resposta := Trim(ACBrValidador.ValidarCNPJ(cCNPJEmitente));
      if NaoEstaVazio(Resposta) then
        raise EACBrLibException.Create(ErrCNPJInvalido, Format(SErrLibSATCNPJEmitenteInvalido, [Resposta]));

      cCodigoVinculacao := cCNPJShw + cCNPJEmitente;
      Resposta := SatDM.ACBrSAT1.SSL.CalcHash(cCodigoVinculacao, dgstSHA256, outBase64, True);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.CriarCFe(eArquivoIni: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TRetornoCriarCFe;
  Resposta: Ansistring;
  ArquivoIni: String;
begin
   try
    ArquivoIni := ConverterAnsiParaUTF8(eArquivoIni);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_CriarCFe(' + ArquivoIni + ' )', logCompleto, True)
    else
      GravarLog('SAT_CriarCFe', logNormal);

    SatDM.Travar;
    Resp := TRetornoCriarCFe.Create(Config.TipoResposta, Config.CodResposta);
    try
      Resposta := '';
      SatDM.ACBrSAT1.CFe.Clear;
      SatDM.ACBrSAT1.InicializaCFe;
      SatDM.ACBrSAT1.CFe.LoadFromIni(ArquivoIni);
      SatDM.ACBrSAT1.CFe.GerarXML(True);
      Resp.Processar(SatDM.ACBrSAT1);
      Resposta := Resp.Gerar;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      Resp.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.CriarEnviarCFe(eArquivoIni: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TRetornoEnvio;
  Resposta: Ansistring;
  ArquivoIni: String;
begin
   try
    ArquivoIni := ConverterAnsiParaUTF8(eArquivoIni);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_CriarEnviarCFe(' + ArquivoIni + ' )', logCompleto, True)
    else
      GravarLog('SAT_CriarEnviarCFe', logNormal);

    SatDM.Travar;
    Resp := TRetornoEnvio.Create(Config.TipoResposta, Config.CodResposta);
    try
      Resposta := '';
      SatDM.ACBrSAT1.CFe.Clear;
      SatDM.ACBrSAT1.InicializaCFe;
      SatDM.ACBrSAT1.CFe.LoadFromIni(ArquivoIni);

      Resp.Resultado := SatDM.ACBrSAT1.EnviarDadosVenda;
      Resp.Processar(SatDM.ACBrSAT1);
      Resposta := Resp.Gerar;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      Resp.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.ValidarCFe(eArquivoXml: PChar): longint;
var
  ArquivoXml, Erro: Ansistring;
  Arquivo: TStringList;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_ValidarCFe(' + ArquivoXml + ' )', logCompleto, True)
    else
      GravarLog('SAT_ValidarCFe', logNormal);

    SatDM.Travar;
    try
     Arquivo:= TStringList.Create;
     try
      Arquivo.LoadFromFile(eArquivoXml);
      Erro := '';
      SatDM.ACBrSAT1.ValidarDadosVenda(Arquivo.Text, Erro);
      Erro := ConverterUTF8ParaAnsi(Erro);
      Result := SetRetorno(ErrOK, Erro);
     finally
       Arquivo.Free;
     end;
    finally
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.EnviarCFe(eArquivoXml: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TRetornoEnvio;
  Resposta: Ansistring;
  ArquivoXml: Ansistring;
begin
   try
    ArquivoXml := ConverterAnsiParaUTF8(eArquivoXml);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_EnviarCFe(' + ArquivoXml + ' )', logCompleto, True)
    else
      GravarLog('SAT_EnviarCFe', logNormal);

    SatDM.Travar;
    Resp := TRetornoEnvio.Create(Config.TipoResposta, Config.CodResposta);
    try
      Resposta := '';
      SatDM.ACBrSAT1.CFe.Clear;
      SatDM.ACBrSAT1.InicializaCFe;
      SatDM.CarregarDadosVenda(ArquivoXml);

      Resp.Resultado := SatDM.ACBrSAT1.EnviarDadosVenda;
      Resp.Processar(SatDM.ACBrSAT1);

      Resposta := Resp.Gerar;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      Resp.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.CancelarCFe(eArquivoXml: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TRetornoCancelarCFe;
  Resposta: Ansistring;
  ArquivoXml: String;
begin
   try
    ArquivoXml := ConverterAnsiParaUTF8(eArquivoXml);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_CancelarCFe(' + ArquivoXml + ' )', logCompleto, True)
    else
      GravarLog('SAT_CancelarCFe', logNormal);

    SatDM.Travar;
    Resp := TRetornoCancelarCFe.Create(Config.TipoResposta, Config.CodResposta);
    try
      Resposta := '';

      SatDM.ACBrSAT1.CFe.Clear;
      SatDM.ACBrSAT1.InicializaCFe;
      SatDM.CarregarDadosVenda(ArquivoXml);

      Resp.Resultado := SatDM.ACBrSAT1.CancelarUltimaVenda;
      Resp.Processar(SatDM.ACBrSAT1);
      Resposta := Resp.Gerar;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      Resp.Free;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.ImprimirExtratoVenda(eArqXMLVenda, eNomeImpressora: PChar): longint;
var
  ArquivoXml, NomeImpressora: String;
begin
   try
    ArquivoXml := ConverterAnsiParaUTF8(eArqXMLVenda);
    NomeImpressora := ConverterAnsiParaUTF8(eNomeImpressora);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_ImprimirExtratoVenda(' + ArquivoXml + ',' + NomeImpressora + ' )', logCompleto, True)
    else
      GravarLog('SAT_ImprimirExtratoVenda', logNormal);

    SatDM.Travar;

    try
      SatDM.ACBrSAT1.CFe.Clear;
      SatDM.CarregarDadosVenda(ArquivoXml);
      SatDM.ConfigurarImpressao(NomeImpressora);
      SatDM.ACBrSAT1.ImprimirExtrato;
      SatDM.ACBrSAT1.Extrato := nil;
      Result := SetRetorno(ErrOK);
    finally
      SatDM.FinalizarImpressao;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.ImprimirExtratoResumido(eArqXMLVenda, eNomeImpressora: PChar): longint;
var
  ArquivoXml, NomeImpressora: String;
begin
   try
    ArquivoXml := String(eArqXMLVenda);
    NomeImpressora := String(eNomeImpressora);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_ImprimirExtratoResumido(' + ArquivoXml + ',' + NomeImpressora + ' )', logCompleto, True)
    else
      GravarLog('SAT_ImprimirExtratoResumido', logNormal);

    SatDM.Travar;

    try
      SatDM.ACBrSAT1.CFe.Clear;
      SatDM.CarregarDadosVenda(ArquivoXml);
      SatDM.ConfigurarImpressao(NomeImpressora);
      SatDM.ACBrSAT1.ImprimirExtratoResumido;
      SatDM.ACBrSAT1.Extrato := nil;
      Result := SetRetorno(ErrOK);
    finally
      SatDM.FinalizarImpressao;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.ImprimirExtratoCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeImpressora: PChar): longint;
var
  ArqXMLVenda, ArqXMLCancelamento, NomeImpressora: String;
begin
   try
    ArqXMLVenda := ConverterAnsiParaUTF8(eArqXMLVenda);
    ArqXMLCancelamento := ConverterAnsiParaUTF8(eArqXMLCancelamento);
    NomeImpressora := ConverterAnsiParaUTF8(eNomeImpressora);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_ImprimirExtratoCancelamento(' + ArqXMLVenda + ',' + ArqXMLCancelamento +
                ',' + NomeImpressora + ' )', logCompleto, True)
    else
      GravarLog('SAT_ImprimirExtratoCancelamento', logNormal);

    SatDM.Travar;

    try
      SatDM.ACBrSAT1.CFe.Clear;
      SatDM.CarregarDadosVenda(ArqXMLVenda);
      SatDM.CarregarDadosCancelamento(ArqXMLCancelamento);
      SatDM.ConfigurarImpressao(NomeImpressora);
      SatDM.ACBrSAT1.ImprimirExtratoCancelamento;
      SatDM.ACBrSAT1.Extrato := nil;
      Result := SetRetorno(ErrOK);
    finally
      SatDM.FinalizarImpressao;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.GerarImpressaoFiscalMFe(eArqXMLVenda: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  ArquivoXml,  Resposta: String;
begin
   try
    ArquivoXml := ConverterAnsiParaUTF8(eArqXMLVenda);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_GerarImpressaoFiscalMFe(' + ArquivoXml + ' )', logCompleto, True)
    else
      GravarLog('SAT_GerarImpressaoFiscalMFe', logNormal);


    SatDM.Travar;

    try
      Resposta := '';
      SatDM.CarregarDadosVenda(ArquivoXml);
      Resposta := SatDM.GerarImpressaoFiscalMFe;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);

      Result := SetRetorno(ErrOK, Resposta);
    finally
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.GerarPDFExtratoVenda(eArqXMLVenda, eNomeArquivo: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
var
  Resp: TPadraoSATResposta;
  ArqXMLVenda, NomeArquivo, Resposta: String;
begin
  try
    ArqXMLVenda := ConverterAnsiParaUTF8(eArqXMLVenda);
    NomeArquivo := ConverterAnsiParaUTF8(eNomeArquivo);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_GerarPDFExtratoVenda(' + ArqXMLVenda + ',' + NomeArquivo + ' )', logCompleto, True)
    else
      GravarLog('SAT_GerarPDFExtratoVenda', logNormal);


    SatDM.Travar;
    Resp := TPadraoSATResposta.Create('CFe', Config.TipoResposta, Config.CodResposta);
    try
      Resposta := '';
      SatDM.CarregarDadosVenda(ArqXMLVenda);
      SatDM.ConfigurarImpressao('', True, NomeArquivo);

      SatDM.ACBrSAT1.ImprimirExtrato;

      Resp.Arquivo:= SatDM.ACBrSAT1.Extrato.ArquivoPDF;
      Resp.XML:= SatDM.ACBrSAT1.CFe.XMLOriginal;
      Resposta := Resp.Gerar;

      SatDM.ACBrSAT1.Extrato := nil;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      Resp.Free;
      SatDM.FinalizarImpressao;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.GerarPDFCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeArquivo: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
var
  Resp: TPadraoSATResposta;
  ArqXMLVenda, ArqXMLCancelamento, NomeArquivo, Resposta: String;
begin
   try
    ArqXMLVenda := ConverterAnsiParaUTF8(eArqXMLVenda);
    ArqXMLCancelamento := ConverterAnsiParaUTF8(eArqXMLCancelamento);
    NomeArquivo := ConverterAnsiParaUTF8(eNomeArquivo);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_GerarPDFCancelamento(' + ArqXMLVenda + ',' + ArqXMLCancelamento +
                ',' + NomeArquivo + ' )', logCompleto, True)
    else
      GravarLog('SAT_GerarPDFCancelamento', logNormal);

    SatDM.Travar;
    Resp := TPadraoSATResposta.Create('CFe', Config.TipoResposta, Config.CodResposta);
    try
      Resposta := '';
      SatDM.CarregarDadosVenda(ArqXMLVenda);
      SatDM.CarregarDadosCancelamento(ArqXMLCancelamento);
      SatDM.ConfigurarImpressao('', True, NomeArquivo);

      SatDM.ACBrSAT1.ImprimirExtratoCancelamento;

      Resp.Arquivo:= SatDM.ACBrSAT1.Extrato.ArquivoPDF;
      Resp.XML:= SatDM.ACBrSAT1.CFeCanc.XMLOriginal;
      Resposta := Resp.Gerar;

      SatDM.ACBrSAT1.Extrato := nil;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      SatDM.FinalizarImpressao;
      SatDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibSAT.EnviarEmail(eArqXMLVenda, sPara, sAssunto, eNomeArquivo, sMensagem,
  sCC, eAnexos: PChar): longint;
var
  ArqXMLVenda, Para, Assunto, NomeArquivo, Mensagem, CC, Anexos: String;
  slMensagem, slCC, slAnexos: TStrings;
begin
   try
    ArqXMLVenda := ConverterAnsiParaUTF8(eArqXMLVenda);
    Para := ConverterAnsiParaUTF8(sPara);
    Assunto := ConverterAnsiParaUTF8(sAssunto);
    NomeArquivo := ConverterAnsiParaUTF8(eNomeArquivo);
    Mensagem := ConverterAnsiParaUTF8(sMensagem);
    CC := ConverterAnsiParaUTF8(sCC);
    Anexos := ConverterAnsiParaUTF8(eAnexos);

    if Config.Log.Nivel > logNormal then
      GravarLog('SAT_EnviarEmail(' + ArqXMLVenda + ',' + Para + ',' + Assunto
                + ',' + NomeArquivo + ',' + Mensagem + ',' + CC + ',' + Anexos + ' )', logCompleto, True)
    else
      GravarLog('SAT_EnviarEmail', logNormal);


    SatDM.Travar;

    slMensagem := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;

     try
       slMensagem.Text := Mensagem;
       slCC.Text := CC;
       slAnexos.Text := Anexos;

       SatDM.CarregarDadosVenda(ArqXMLVenda);
       if (NomeArquivo <> '') then
       begin
         SatDM.ConfigurarImpressao('', True, NomeArquivo);
         SatDM.ACBrSAT1.ImprimirExtrato;
         slAnexos.Add(SatDM.ACBrSAT1.Extrato.ArquivoPDF);
       end;

       SatDM.ACBrSAT1.EnviarEmail(Para, Assunto, slMensagem, slCC, slAnexos);
       Result := SetRetorno(ErrOK);
     finally
       slMensagem.Free;
       slCC.Free;
       slAnexos.Free;
       SatDM.FinalizarImpressao;
       SatDM.Destravar;
     end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

end.

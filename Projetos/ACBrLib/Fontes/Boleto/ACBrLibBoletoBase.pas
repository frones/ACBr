{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: José M. S. Junior                               }
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

unit ACBrLibBoletoBase;


interface

uses
  Classes, SysUtils,
  ACBrLibComum,
  ACBrLibBoletoDataModule, ACBrBoletoConversao;

type

  {TACBrLibBoleto}
  TACBrLibBoleto = class(TACBrLib)
  private
    FBoletoDM: TLibBoletoDM;

    function ListaBancos: AnsiString;
    function ListaCaractTitulo : AnsiString;
    function ListaOcorrencias: AnsiString;
    function ListaOcorrenciasEX: AnsiString;

  protected
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;
    procedure AtualizaLayoutImpressao;

  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property BoletoDM: TLibBoletoDM read FBoletoDM;

    function ConfigurarDados(eArquivoIni: PAnsiChar): Integer;
    function IncluirTitulos(eArquivoIni, eTpSaida: PAnsiChar): Integer;
    function LimparLista: Integer;
    function TotalTitulosLista: Integer;
    function Imprimir(eNomeImpressora: PAnsiChar): Integer;
    function ImprimirBoleto(eIndice: Integer; eNomeImpressora: PAnsiChar): Integer;
    function GerarPDF: Integer;
    function SalvarPDF(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function GerarPDFBoleto(eIndice: Integer): Integer;
    function SalvarPDFBoleto(eIndice: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function GerarHTML: Integer;
    function GerarRemessa(eDir: PAnsiChar; eNumArquivo: Integer; eNomeArq: PAnsiChar): Integer;
    function GerarRemessaStream(eNumArquivo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function LerRetorno(eDir, eNomeArq: PAnsiChar): Integer;
    function LerRetornoStream(const ARetornoBase64: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function ObterRetorno(eDir, eNomeArq: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function EnviarEmail(ePara, eAssunto, eMensagem, eCC: PAnsiChar): Integer;
    function EnviarEmailBoleto(eIndice: Integer; ePara, eAssunto, eMensagem, eCC: PAnsiChar): Integer;
    function SetDiretorioArquivo(eDir, eArq: PAnsiChar): Integer;
    function ListaBancos(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function ListaCaractTitulo(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function ListaOcorrencias(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function ListaOcorrenciasEX(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function TamNossoNumero(eCarteira, enossoNumero, eConvenio: PAnsiChar): Integer;
    function CodigosMoraAceitos(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function SelecionaBanco(eCodBanco: PAnsiChar): Integer;
    function MontarNossoNumero(eIndice: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function RetornaLinhaDigitavel(eIndice: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function RetornaCodigoBarras(eIndice: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function EnviarBoleto(eCodigoOperacao: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function ConsultarTitulosPorPeriodo(eArquivoIni: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;

  end;

implementation

uses
  strutils, typinfo,
  synacode,
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrMail, ACBrBoleto,
  ACBrLibConsts, ACBrLibBoletoConsts, ACBrLibConfig,
  ACBrLibResposta, ACBrLibBoletoConfig, ACBrLibBoletoRespostas;
  
constructor TACBrLibBoleto.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited
  Create(ArqConfig, ChaveCrypt);
  FBoletoDM := TLibBoletoDM.Create(nil);
  FBoletoDM.Lib := Self;
end;

destructor TACBrLibBoleto.Destroy;
begin
  FBoletoDM.Free;
  inherited Destroy;
end;

procedure TACBrLibBoleto.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibBoletoConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibBoleto.Executar;
begin
  inherited Executar;
  FBoletoDM.AplicarConfiguracoes;
end;

procedure TACBrLibBoleto.AtualizaLayoutImpressao;
begin
  if Assigned(BoletoDM.ACBrBoleto1.ACBrBoletoFC) then
  begin
    if (TLibBoletoConfig(Config).BoletoFCFortesConfig.Layout <> BoletoDM.ACBrBoleto1.ACBrBoletoFC.LayOut) then
     BoletoDM.LayoutImpressao:= Integer(BoletoDM.ACBrBoleto1.ACBrBoletoFC.LayOut);
  end;
end;

function TACBrLibBoleto.ConfigurarDados(eArquivoIni: PAnsiChar): Integer;
var
  ArquivoIni: AnsiString;
begin
  try
    ArquivoIni := ConverterStringEntrada(eArquivoIni);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_ConfigurarDados(' + ArquivoIni + ' )', logCompleto, True)
    else
      GravarLog('Boleto_ConfigurarDados', logNormal);

    BoletoDM.Travar;
    try
      BoletoDM.ConfigurarImpressao; 
      try
        if not (BoletoDM.ACBrBoleto1.LerArqIni( ArquivoIni )) then
         raise EACBrLibException.Create(ErrConfigLer, Format(SErroLerArquivoEntrada, [ArquivoIni]));

        AtualizaLayoutImpressao;

        Result := SetRetorno(ErrOK);

      finally
        BoletoDM.FinalizarImpressao;
      end;

    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.IncluirTitulos(eArquivoIni, eTpSaida: PAnsiChar): Integer;
var
  ArquivoIni, TpSaida : AnsiString;
  Mensagem : TStringList;
begin
  try
    ArquivoIni := ConverterStringEntrada(eArquivoIni);
    TpSaida := ConverterStringEntrada(eTpSaida);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_IncluirTitulos(' + ArquivoIni + ', ' + TpSaida + ' )', logCompleto, True)
    else
      GravarLog('Boleto_IncluirTitulos', logNormal);

    BoletoDM.Travar;

    try
{$IFDEF Demo}
    if BoletoDM.ACBrBoleto1.ListadeBoletos.Count = 5 then
    begin
      Result := SetRetorno(ErrDemoExpirado,  'So pode adicionar 5 boletos na versão Demo.');
      Exit;
    end;
{$ENDIF}
      BoletoDM.ConfigurarImpressao;

      try
        if not (BoletoDM.ACBrBoleto1.LerArqIni( ArquivoIni )) then
         raise EACBrLibException.Create(ErrConfigLer, Format(SErroLerArquivoEntrada, [ArquivoIni]));

        AtualizaLayoutImpressao;

        if TpSaida = 'I' then
        begin
          BoletoDM.ACBrBoleto1.Imprimir
        end
        else if TpSaida = 'P' then
        begin
          BoletoDM.ACBrBoleto1.GerarPDF
        end
        else if TpSaida = 'E' then
        begin
          with TLibBoletoConfig(Config).BoletoConfig do
          begin
            Mensagem := TStringList.Create;
            Mensagem.Duplicates := dupAccept;

            try
              Mensagem.Add(emailMensagemBoleto);
              if Config.Log.Nivel > logNormal then
                GravarLog('Boleto_EnviarEmail(' + BoletoDM.ACBrBoleto1.ListadeBoletos[0].Sacado.Email +
                          ',' + emailAssuntoBoleto + ',' + Mensagem.Text, logCompleto, True)
              else
                GravarLog('Boleto_EnviarEmail', logNormal);

              BoletoDM.ACBrBoleto1.EnviarEmail( BoletoDM.ACBrBoleto1.ListadeBoletos[0].Sacado.Email,
                                                emailAssuntoBoleto, Mensagem, True );
            finally
              Mensagem.Free;
            end;
          end;
        end;
      finally
         BoletoDM.FinalizarImpressao;
      end;

      Result := SetRetorno(ErrOk, Format('%d Titulo(s) Carregado(s)', [BoletoDM.ACBrBoleto1.ListadeBoletos.Count]));
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.LimparLista: Integer;  
begin
  try
    GravarLog('Boleto_LimparLista', logNormal);

    BoletoDM.Travar;
    try
      BoletoDM.ACBrBoleto1.ListadeBoletos.Clear;
      Result := SetRetorno(ErrOK);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.TotalTitulosLista: Integer;
var
  Resposta: AnsiString;
begin
  try
    GravarLog('Boleto_TotalTitulosLista', logNormal);

    BoletoDM.Travar;
    try
      Result := SetRetorno(BoletoDM.ACBrBoleto1.ListadeBoletos.Count);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.Imprimir(eNomeImpressora: PAnsiChar): Integer;  
var
  NomeImpressora : AnsiString;
begin
  try
    NomeImpressora := ConverterStringEntrada(eNomeImpressora);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_Imprimir(' + NomeImpressora + ' )', logCompleto, True)
    else
      GravarLog('Boleto_Imprimir', logNormal);

    BoletoDM.Travar;
    try
      BoletoDM.ConfigurarImpressao(NomeImpressora);
      BoletoDM.ACBrBoleto1.Imprimir;
      Result := SetRetorno(ErrOK);
    finally
      BoletoDM.FinalizarImpressao;
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.ImprimirBoleto(eIndice: Integer; eNomeImpressora: PAnsiChar): Integer;  
var
  NomeImpressora : AnsiString;
begin
  try
    NomeImpressora := ConverterStringEntrada(eNomeImpressora);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_ImprimirBoleto(' + IntToStr(eIndice) + ', ' + NomeImpressora + ' )', logCompleto, True)
    else
      GravarLog('Boleto_ImprimirBoleto', logNormal);

    BoletoDM.Travar;
    try
      BoletoDM.ConfigurarImpressao(NomeImpressora);
      BoletoDM.ACBrBoleto1.Imprimir(eIndice);
      Result := SetRetorno(ErrOK);
    finally
      BoletoDM.FinalizarImpressao;
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.GerarPDF: Integer;  
begin
  try
    GravarLog('Boleto_GerarPDF', logNormal);

    BoletoDM.Travar;
    try
      BoletoDM.ConfigurarImpressao;
      BoletoDM.ACBrBoleto1.GerarPDF;
      Result := SetRetorno(ErrOK);
    finally
      BoletoDM.FinalizarImpressao;
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.SalvarPDF(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
Var
  AStream: TMemoryStream;
  Resposta: Ansistring;
begin
  try
    GravarLog('Boleto_SalvarPDF', logNormal);

    AStream := TMemoryStream.Create;

    BoletoDM.Travar;
    try
      BoletoDM.ConfigurarImpressao;
      BoletoDM.ACBrBoleto1.GerarPDF(AStream);

      Resposta := StreamToBase64(AStream);

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.FinalizarImpressao;
      AStream.Free;
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.GerarPDFBoleto(eIndice: Integer): Integer;
begin
  try
      if Config.Log.Nivel > logNormal then
        GravarLog('Boleto_GerarPDFBoleto(' + IntToStr(eIndice) + ' )', logCompleto, True)
      else
        GravarLog('Boleto_GerarPDFBoleto', logNormal);

      BoletoDM.Travar;
      try
        BoletoDM.ConfigurarImpressao;
        BoletoDM.ACBrBoleto1.GerarPDF(eIndice);
        Result := SetRetorno(ErrOK);
      finally
        BoletoDM.FinalizarImpressao;
        BoletoDM.Destravar;
      end;
    except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
    end;
end;

function TACBrLibBoleto.SalvarPDFBoleto(eIndice: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
Var
  AStream: TMemoryStream;
  Resposta: Ansistring;
begin
  try
      if Config.Log.Nivel > logNormal then
        GravarLog('Boleto_SalvarPDFBoleto(' + IntToStr(eIndice) + ' )', logCompleto, True)
      else
        GravarLog('Boleto_SalvarPDFBoleto', logNormal);

      AStream := TMemoryStream.Create;

      BoletoDM.Travar;
      try
        BoletoDM.ConfigurarImpressao;
        BoletoDM.ACBrBoleto1.GerarPDF(eIndice, AStream);

        Resposta := StreamToBase64(AStream);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        BoletoDM.FinalizarImpressao;
        AStream.Free;
        BoletoDM.Destravar;
      end;
    except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
    end;
end;

function TACBrLibBoleto.GerarHTML: Integer;  
begin
  try
    GravarLog('Boleto_GerarHTML', logNormal);

    BoletoDM.Travar;
    try
      BoletoDM.ConfigurarImpressao;
      BoletoDM.ACBrBoleto1.GerarHTML;
      Result := SetRetorno(ErrOK);
    finally
      BoletoDM.FinalizarImpressao;
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.GerarRemessa(eDir: PAnsiChar; eNumArquivo: Integer; eNomeArq: PAnsiChar): Integer;  
var
  Dir, NomeArq: AnsiString;
  NumArquivo: Integer;
begin
  try
    Dir := ConverterStringEntrada(eDir);
    NumArquivo:= StrToIntDef(IntToStr(eNumArquivo ), 0);
    NomeArq:= ConverterStringEntrada(eNomeArq);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_GerarRemessa(' + Dir + ', ' + IntToStr(NumArquivo) + ', ' + NomeArq + ' )', logCompleto, True)
    else
      GravarLog('Boleto_GerarRemessa', logNormal);

    BoletoDM.Travar;
    try
      if NaoEstaVazio( Dir ) then
        BoletoDM.ACBrBoleto1.DirArqRemessa := Dir;
      if NaoEstaVazio( NomeArq ) then
        BoletoDM.ACBrBoleto1.NomeArqRemessa:= NomeArq;

      BoletoDM.ACBrBoleto1.GerarRemessa( NumArquivo );
      Result := SetRetorno(ErrOK);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.GerarRemessaStream(eNumArquivo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  NumArquivo: Integer;
  AStream: TMemoryStream;
  Resposta: Ansistring;
begin
  try
    NumArquivo:= StrToIntDef(IntToStr(eNumArquivo ), 0);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_GerarRemessaStream(' + IntToStr(NumArquivo) + ' )', logCompleto, True)
    else
      GravarLog('Boleto_GerarRemessaStream', logNormal);

    AStream := TMemoryStream.Create;
    BoletoDM.Travar;

    try
      BoletoDM.ACBrBoleto1.GerarRemessaStream( NumArquivo, AStream );
      Resposta := StreamToBase64(AStream);

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      AStream.Free;
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.LerRetorno(eDir, eNomeArq: PAnsiChar): Integer;  
var
  Dir, NomeArq: AnsiString;
begin
  try
    Dir := ConverterStringEntrada(eDir);
    NomeArq:= ConverterStringEntrada(eNomeArq);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_LerRetorno(' + Dir + ', ' + NomeArq + ' )', logCompleto, True)
    else
      GravarLog('Boleto_LerRetorno', logNormal);

    BoletoDM.Travar;
    try
      if NaoEstaVazio( Dir ) then
        BoletoDM.ACBrBoleto1.DirArqRetorno := Dir;
      if NaoEstaVazio( NomeArq ) then
        BoletoDM.ACBrBoleto1.NomeArqRetorno:= NomeArq;

      BoletoDM.ACBrBoleto1.LerRetorno();
      BoletoDM.ACBrBoleto1.GravarArqIni(Dir,'');
      Result := SetRetorno(ErrOK);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.LerRetornoStream(const ARetornoBase64: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Stream: TStringStream;
  Base64, Resposta : Ansistring;
begin
  try
    GravarLog('Boleto_LerRetornoStream', logNormal);
    Base64 := ACBrUTF8ToAnsi(DecodeBase64(ARetornoBase64));
    Stream := TStringStream.Create(Base64);
    try
      BoletoDM.Travar;
      try
        BoletoDM.ACBrBoleto1.LerRetorno(Stream);

        Resposta := ACBrUTF8ToAnsi(BoletoDM.ACBrBoleto1.GravarArqIni('','',False));
        Resposta := EncodeBase64(Resposta);

        MoverStringParaPChar (Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        BoletoDM.Destravar;
      end;
    finally
      Stream.Free;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.ObterRetorno(eDir, eNomeArq: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Dir, NomeArq: AnsiString;
  Resposta: AnsiString;
  RespRetorno : TRetornoBoleto;
begin
  try
    Dir := ConverterStringEntrada(eDir);
    NomeArq:= ConverterStringEntrada(eNomeArq);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_ObterRetorno(' + eDir + ', ' + eNomeArq + ')', logCompleto, True)
    else
      GravarLog('Boleto_ObterRetorno', logNormal);

    BoletoDM.Travar;
    try
      if NaoEstaVazio( Dir ) then
        BoletoDM.ACBrBoleto1.DirArqRetorno := Dir;
      if NaoEstaVazio( NomeArq ) then
        BoletoDM.ACBrBoleto1.NomeArqRetorno:= NomeArq;

      BoletoDM.ACBrBoleto1.LerRetorno();

      RespRetorno := TRetornoBoleto.Create(Config.TipoResposta, Config.CodResposta);
      try
        RespRetorno.Processar(BoletoDM.ACBrBoleto1);
        Resposta := ACBrStr(RespRetorno.Gerar);
        Resposta := RespRetorno.Gerar;
      Finally
        RespRetorno.Free;
      end;

      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta );
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);

    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;

end;

function TACBrLibBoleto.EnviarEmail(ePara, eAssunto, eMensagem, eCC: PAnsiChar): Integer;  
var
  Para, Assunto, Mensagem, CC: AnsiString;
  slMensagem, slCC: TStrings;
begin
  try
    Para := ConverterStringEntrada(ePara);
    Assunto := ConverterStringEntrada(eAssunto);
    Mensagem := ConverterStringEntrada(eMensagem);
    CC := ConverterStringEntrada(eCC);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_EnviarEmail(' + Para + ', ' + Assunto + ', ' + Mensagem + ', ' + CC +')', logCompleto, True)
    else
      GravarLog('Boleto_EnviarEmail', logNormal);

    BoletoDM.Travar;

    if EstaVazio(ePara) and (BoletoDM.ACBrBoleto1.ListadeBoletos.Count > 0) then
      Para := BoletoDM.ACBrBoleto1.ListadeBoletos[0].Sacado.Email;

    try
      slMensagem := TStringList.Create;
      slMensagem.Text := Mensagem;

      slCC := TStringList.Create;
      slCC.Text := CC;

      BoletoDM.ConfigurarImpressao;
      BoletoDM.ACBrBoleto1.EnviarEmail(Para, Assunto, slMensagem, True, slCC);
      Result := SetRetorno(ErrOK);
    finally
      slMensagem.Free;
      slCC.Free;
      BoletoDM.FinalizarImpressao;
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.EnviarEmailBoleto(eIndice: Integer; ePara, eAssunto, eMensagem, eCC: PAnsiChar): Integer;
var
  Para, Assunto, Mensagem, CC: AnsiString;
  slMensagem, slCC: TStrings;
begin
  try
    Para := ConverterStringEntrada(ePara);
    Assunto := ConverterStringEntrada(eAssunto);
    Mensagem := ConverterStringEntrada(eMensagem);
    CC := ConverterStringEntrada(eCC);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_EnviarEmailBoleto(' + IntToStr(eIndice) + ', ' + Para + ', ' + Assunto
      + ', ' + Mensagem + ', ' + CC +')', logCompleto, True)
    else
      GravarLog('Boleto_EnviarEmailBoleto', logNormal);

    BoletoDM.Travar;

    if eIndice > (BoletoDM.ACBrBoleto1.ListadeBoletos.Count -1) then
      raise Exception.Create('Título de Indice '+IntToStr(eIndice)+' não identificado na Lista!');

    if EstaVazio(ePara) and (BoletoDM.ACBrBoleto1.ListadeBoletos.Count > 0) then
      Para := BoletoDM.ACBrBoleto1.ListadeBoletos[eIndice].Sacado.Email;

    try
      slMensagem := TStringList.Create;
      slMensagem.Text := Mensagem;

      slCC := TStringList.Create;
      slCC.Text := CC;

      BoletoDM.ConfigurarImpressao;
      BoletoDM.ACBrBoleto1.ListadeBoletos[eIndice].EnviarEmail(Para, Assunto, slMensagem, True, slCC);
      Result := SetRetorno(ErrOK);
    finally
      slMensagem.Free;
      slCC.Free;
      BoletoDM.FinalizarImpressao;
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.SetDiretorioArquivo(eDir, eArq: PAnsiChar): Integer;
var
  Dir, Arq : AnsiString;
begin
   try
     Dir := ConverterStringEntrada(eDir);
     Arq := ConverterStringEntrada(eArq);

     if Config.Log.Nivel > logNormal then
       GravarLog('Boleto_SetDiretorioArquivo(' + Dir + ', ' + Arq + ' )', logCompleto, True)
     else
       GravarLog('Boleto_SetDiretorioArquivo', logNormal);

     if not DirectoryExists(Dir) then
       raise  EACBrLibException.Create(ErrDiretorioNaoExiste, 'Diretorio não existe');

     BoletoDM.Travar;
     try
       if TLibBoletoConfig(Config).BoletoFCFortesConfig.Filtro = TACBrBoletoFCFiltro(fiHTML) then
         TLibBoletoConfig(Config).BoletoFCFortesConfig.NomeArquivo := PathWithDelim( Dir )  +
         IfThen(NaoEstaVazio(Arq), Arq , 'boleto.html' )
       else
         TLibBoletoConfig(Config).BoletoFCFortesConfig.NomeArquivo := PathWithDelim( Dir ) +
         IfThen(NaoEstaVazio(Arq), Arq , 'boleto.pdf' );

       Result := SetRetorno(ErrOK);
     finally
       BoletoDM.Destravar;
     end;
   except
     on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
     on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
   end;
end;

function TACBrLibBoleto.ListaBancos(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;  
var
   Resposta : AnsiString;
begin
  try
    GravarLog('Boleto_ListaBancos', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      Resposta := ListaBancos;
      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.ListaCaractTitulo(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;  
var
   Resposta : AnsiString;
begin
  try
    GravarLog('Boleto_ListaCaractTitulo', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      Resposta := ListaCaractTitulo();
      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.ListaOcorrencias(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;  
var
   Resposta : AnsiString;
begin
  try
    GravarLog('Boleto_ListaOcorrencias', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      Resposta := ListaOcorrencias();
      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.ListaOcorrenciasEX(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;  
var
   Resposta : AnsiString;
begin
  try
    GravarLog('Boleto_ListaOcorrenciasEX', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      Resposta := ListaOcorrenciasEX();
      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.TamNossoNumero(eCarteira, enossoNumero, eConvenio: PAnsiChar): Integer;
var
   Carteira, NossoNumero, Convenio : AnsiString;
begin
  try
    Carteira := ConverterStringEntrada(eCarteira);
    NossoNumero:= ConverterStringEntrada(enossoNumero);
    Convenio:= ConverterStringEntrada(eConvenio);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_TamNossoNumero(' + eCarteira + ', ' + enossoNumero + ', ' +  eConvenio + ')', logCompleto, True)
    else
      GravarLog('Boleto_TamNossoNumero', logNormal);

    BoletoDM.Travar;
    try
      Result := SetRetorno(BoletoDM.ACBrBoleto1.Banco.CalcularTamMaximoNossoNumero(Carteira, NossoNumero, Convenio));
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.CodigosMoraAceitos(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;  
var
  Resposta : AnsiString;
begin
  try
    GravarLog('Boleto_CodigosMoraAceitos', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      Resposta := BoletoDM.ACBrBoleto1.Banco.CodigosMoraAceitos;
      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.SelecionaBanco(eCodBanco: PAnsiChar): Integer;
var
   CodBanco : AnsiString;
begin
  try
    CodBanco := ConverterStringEntrada(eCodBanco);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_SelecionaBanco(' + eCodBanco + ' )', logCompleto, True)
    else
      GravarLog('Boleto_SelecionaBanco', logNormal);

    BoletoDM.Travar;

    try
      BoletoDM.ACBrBoleto1.Banco.TipoCobranca := BoletoDM.ACBrBoleto1.GetTipoCobranca(StrToInt64Def(Trim(CodBanco),0));
      Result := SetRetorno(ErrOK);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.MontarNossoNumero(eIndice: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
   Resposta : AnsiString;
   Indice : Integer;
begin
  try
    Indice := 0;
    if (eIndice >= 0) then
      Indice := eIndice;

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_MontarNossoNumero(' + IntToStr(eIndice) + ' )', logCompleto, True)
    else
      GravarLog('Boleto_MontarNossoNumero', logNormal);

    if (BoletoDM.ACBrBoleto1.ListadeBoletos.Count = 0) then
        raise  EACBrLibException.Create(ErrIndex, 'Titulo não encontrado.');

    BoletoDM.Travar;
    try
      Resposta := '';
      Resposta := BoletoDM.ACBrBoleto1.Banco.MontarCampoNossoNumero(BoletoDM.ACBrBoleto1.ListadeBoletos[Indice]);
      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.RetornaLinhaDigitavel(eIndice: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
   Resposta : AnsiString;
   Indice : Integer;
   ABarras : String;
begin
  try
    Indice := 0;
    if (eIndice >= 0) then
      Indice := eIndice;

    if Config.Log.Nivel > logNormal then
       GravarLog('Boleto_RetornaLinhaDigitavel(' + IntToStr(eIndice) + ' )', logCompleto, True)
     else
       GravarLog('Boleto_RetornaLinhaDigitavel', logNormal);

    if (BoletoDM.ACBrBoleto1.ListadeBoletos.Count = 0) then
      raise  EACBrLibException.Create(ErrIndex, 'Titulo não encontrado.');

    BoletoDM.Travar;

    try
      Resposta := '';
      ABarras  := BoletoDM.ACBrBoleto1.Banco.MontarCodigoBarras(BoletoDM.ACBrBoleto1.ListadeBoletos[Indice]);
      Resposta := BoletoDM.ACBrBoleto1.Banco.MontarLinhaDigitavel(ABarras, BoletoDM.ACBrBoleto1.ListadeBoletos[Indice]);
      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.RetornaCodigoBarras(eIndice: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
   Resposta : AnsiString;
   Indice : Integer;
begin
  try
    Indice := 0;
    if (eIndice >= 0) then
      Indice := eIndice;

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_RetornaCodigoBarras(' + IntToStr(eIndice) + ' )', logCompleto, True)
    else
      GravarLog('Boleto_RetornaCodigoBarras', logNormal);

    if (BoletoDM.ACBrBoleto1.ListadeBoletos.Count = 0) then
      raise  EACBrLibException.Create(ErrIndex, 'Titulo não encontrado.');

    BoletoDM.Travar;
    try
      Resposta := '';
      Resposta := BoletoDM.ACBrBoleto1.Banco.MontarCodigoBarras(BoletoDM.ACBrBoleto1.ListadeBoletos[Indice]);
      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.ListaBancos: AnsiString;
var
   SBanco : String;
   I: Integer;
begin
   Result := '';
   for i:= integer( Low(TACBrTipoCobranca) ) + 1  to integer( High(TACBrTipoCobranca) ) do
   begin
     sBanco := GetEnumName( TypeInfo(TACBrTipoCobranca), Integer(I) );
     sBanco := copy(SBanco,4, Length(SBanco)); // Removendo "cob" do nome do banco.
     Result := Result + sBanco + '|';
   end;

   if Result <> '' then
      Result := copy(Result,1,Length(Result)-1) ;
end;

function TACBrLibBoleto.ListaCaractTitulo: AnsiString;
var
   ICaractTitulo : TACBrCaracTitulo;
   SCaractTitulo : String;
begin

   for ICaractTitulo := Low(TACBrCaracTitulo) to high(TACBrCaracTitulo) do
   begin
     SCaractTitulo := GetEnumName( TypeInfo(TACBrCaracTitulo), Integer(ICaractTitulo) );
     SCaractTitulo := copy(SCaractTitulo, 3, Length(SCaractTitulo)); // Removendo "tc".
     Result := Result + SCaractTitulo + '|';
   end;

   if Result <> '' then
      Result := copy(Result,1,Length(Result)-1) ;
end;

function TACBrLibBoleto.ListaOcorrencias: AnsiString;
var
   ITipoOcorrencia : TACBrTipoOcorrencia;
   SOcorrencia     : String;
begin
  for ITipoOcorrencia := Low(TACBrTipoOcorrencia) to High(TACBrTipoOcorrencia) do
  begin
    SOcorrencia := GetEnumName( TypeInfo(TACBrTipoOcorrencia), Integer(ITipoOcorrencia) ) ;
    Result := Result + copy(SOcorrencia, 3, Length(SOcorrencia)) + '|';  //Remove "to"
  end;

  if (Result <> '') then
    Result := copy(Result,1,Length(Result)-1) ;
end;

function TACBrLibBoleto.ListaOcorrenciasEX: AnsiString;
var
   ITipoOcorrencia : TACBrTipoOcorrencia;
   SOcorrencia     : String;
   ValorIndice     : Integer;
begin

  for ITipoOcorrencia := Low(TACBrTipoOcorrencia) to High(TACBrTipoOcorrencia) do
  begin
    ValorIndice := Integer(ITipoOcorrencia);
    SOcorrencia := GetEnumName( TypeInfo(TACBrTipoOcorrencia), ValorIndice ) ;
    Result := Result + IntToStr(ValorIndice) + '-' +
              copy(SOcorrencia, 3, Length(SOcorrencia)) + '|';  //Remove "to"
  end;

  if (Result <> '') then
    Result := copy(Result, 1, Length(Result)-1) ;
end;

function TACBrLibBoleto.EnviarBoleto(eCodigoOperacao: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Resposta: AnsiString;
  Titulo : TRetornoRegistroWeb;
  I: Integer;
  CodigoOperacao: Integer;
  Titulos: TArray<TRetornoRegistroWeb>;
begin
  CodigoOperacao := 0;
    if (eCodigoOperacao >= 0) then
      CodigoOperacao := eCodigoOperacao;

  try
    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_EnviarBoleto(' + IntToStr(eCodigoOperacao) + ' )', logCompleto, True)
    else
      GravarLog('Boleto_EnviarBoleto', logNormal);

    BoletoDM.Travar;

    try
      BoletoDM.ACBrBoleto1.Configuracoes.WebService.Operacao:= TOperacao(CodigoOperacao);
      BoletoDM.ACBrBoleto1.Enviar;

      if BoletoDM.ACBrBoleto1.TotalListaRetornoWeb > 0 then
      begin
        SetLength(Titulos, BoletoDM.ACBrBoleto1.TotalListaRetornoWeb);
        try
          for I:= 0 to BoletoDM.ACBrBoleto1.TotalListaRetornoWeb -1 do
          begin
            Titulo := TRetornoRegistroWeb.Create(I + 1, Config.TipoResposta, Config.CodResposta);
            Titulo.Processar(BoletoDM.ACBrBoleto1.ListaRetornoWeb[I]);
            Titulos[I] := Titulo;
          end;

          Resposta := GerarResposta<TRetornoRegistroWeb>(Titulos);
        finally
          for I:= 0 to High(Titulos) do
          begin
            Titulo := Titulos[I] as TRetornoRegistroWeb;
            FreeAndNil(Titulo);
          end;

          SetLength(Titulos, 0);
          Titulos := nil;
        end;
      end;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibBoleto.ConsultarTitulosPorPeriodo(eArquivoIni: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  ArquivoIni: AnsiString;
  Resposta: AnsiString;
  Titulo : TRetornoRegistroWeb;
  Titulos: TArray<TRetornoRegistroWeb>;
  I: Integer;
begin
  try
    ArquivoIni := ConverterStringEntrada(eArquivoIni);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_ConsultarTitulosPeriodo(' + ArquivoIni + ' )', logCompleto, True)
    else
      GravarLog('Boleto_ConsultarTitulosPeriodo', logNormal);

    BoletoDM.Travar;
    try
      if not (BoletoDM.ACBrBoleto1.LerArqIni( ArquivoIni )) then
        raise EACBrLibException.Create(ErrConfigLer, Format(SErroLerArquivoEntrada, [ArquivoIni]));

      BoletoDM.ACBrBoleto1.Configuracoes.WebService.Operacao := tpConsulta;
      BoletoDM.ACBrBoleto1.Enviar;

      if BoletoDM.ACBrBoleto1.ListaConsultaRetornoWeb.Count > 0 then
      begin
        SetLength(Titulos, BoletoDM.ACBrBoleto1.ListaConsultaRetornoWeb.Count);
        try
          for I:= 0 to BoletoDM.ACBrBoleto1.ListaConsultaRetornoWeb.Count -1 do
          begin
            Titulo := TRetornoRegistroWeb.Create(I + 1, Config.TipoResposta, Config.CodResposta);
            Titulo.Processar(BoletoDM.ACBrBoleto1.ListaConsultaRetornoWeb[I]);
            Titulos[I] := Titulo;
          end;

          Resposta := GerarResposta<TRetornoRegistroWeb>(Titulos);
        finally
          for I:= 0 to High(Titulos) do
          begin
            Titulo := Titulos[I] as TRetornoRegistroWeb;
            FreeAndNil(Titulo);
          end;

          SetLength(Titulos, 0);
          Titulos := nil;
        end;
      end;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);

    finally
      BoletoDM.Destravar;
    end;

  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;

end;

end.


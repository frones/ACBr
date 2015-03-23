{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
|* 29/11/2010: Gutierres Santana da Costa
|*  - Implementado Registro Tipo C "Controle de Abastecimento e Encerrante"
*******************************************************************************}

{$I ACBr.inc}

unit ACBrPAF;

interface

uses
   SysUtils, Classes, DateUtils,
   {$IFNDEF NOGUI}
    {$IFDEF FPC}LResources,{$ENDIF}
    {$IF DEFINED(CLX)}
       QForms,
    {$ELSEIF DEFINED(FMX)}
       FMX.Forms,
    {$ELSE}
       Forms,
    {$IFEND}
//    {$IFDEF CLX} QForms, {$ELSE} Forms, {$ENDIF}
   {$ENDIF}
   ACBrTXTClass, ACBrUtil, ACBrEAD, ACBrAAC,
   ACBrPAF_A_Class,
   ACBrPAF_B_Class,
   ACBrPAF_C_Class,
   ACBrPAF_D_Class,
   ACBrPAF_E_Class,
   ACBrPAF_F_Class,
   ACBrPAF_G_Class,
   ACBrPAF_H_Class,
   ACBrPAF_L_Class,
   ACBrPAF_M_Class,
   ACBrPAF_N_Class,
   ACBrPAF_P_Class,
   ACBrPAF_R_Class,
   ACBrPAF_S_Class,
   ACBrPAF_T_Class,
   ACBrPAF_TITP_Class,
   ACBrPAF_U_Class;

const
   CACBrPAF_Versao = '0.09' ;

type

  // DECLARANDO O COMPONENTE - PAF-ECF:

  { TACBrPAF }

  TACBrPAF = class(TComponent)
  private
    FOnError: TErrorEvent;

    fsEADInterno : TACBrEAD ;
    fsEAD : TACBrEAD ;       /// Componente usado para AssinarArquivo com assinatura EAD.
    fsAAC : TACBrAAC ;       /// Componente usado para manter o Arq.Auxiliar Criptografado

    FPath: String;            // Path do arquivo a ser gerado
    FDelimitador: String;     // Caracter delimitador de campos
    FTrimString: boolean;     // Retorna a string sem espaços em branco iniciais e finais
    FCurMascara: String;      // Mascara para valores tipo currency
    FAssinar : Boolean;       // Define se o arquivo gerado deve ser assinado

    FPAF_A: TPAF_A;
    FPAF_B: TPAF_B;
    FPAF_C: TPAF_C;
    FPAF_D: TPAF_D;
    FPAF_E: TPAF_E;
    FPAF_F: TPAF_F;
    FPAF_G: TPAF_G;
    FPAF_H: TPAF_H;
    FPAF_L: TPAF_L;
    FPAF_M: TPAF_M;
    FPAF_N: TPAF_N;
    FPAF_P: TPAF_P;
    FPAF_R: TPAF_R;
    FPAF_S: TPAF_S;
    FPAF_T: TPAF_T;
    FPAF_TITP: TPAF_TITP;
    FPAF_U: TPAF_U;
    fsOnPAFCalcEAD: TACBrEADCalc;
    fsOnPAFGetKeyRSA : TACBrEADGetChave ;

    function GetAbout: String;
    function GetDelimitador: String;
    function GetTrimString: boolean;
    function GetCurMascara: String;
    procedure SetDelimitador(const Value: String);
    procedure SetTrimString(const Value: boolean);
    procedure SetCurMascara(const Value: String);

    function GetOnError: TErrorEvent; // Método do evento OnError
    procedure SetOnError(const Value: TErrorEvent); // Método SetError

    procedure SetEAD(const AValue: TACBrEAD);
    procedure SetAAC(const AValue: TACBrAAC);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override; // Create
    destructor Destroy; override; // Destroy

    function SaveFileTXT_B(Arquivo: String): Boolean; // Método que escreve o arquivo texto no caminho passado como parâmetro
    function SaveFileTXT_C(Arquivo: String): Boolean; // Método que escreve o arquivo texto no caminho passado como parâmetro
    function SaveFileTXT_D(Arquivo: String): Boolean; // Método que escreve o arquivo texto no caminho passado como parâmetro
    function SaveFileTXT_E(Arquivo: String): Boolean; // Método que escreve o arquivo texto no caminho passado como parâmetro
    function SaveFileTXT_H(Arquivo: String): Boolean;
    function SaveFileTXT_N(Arquivo: String): Boolean; // Método que escreve o arquivo texto no caminho passado como parâmetro
    function SaveFileTXT_P(Arquivo: String): Boolean; // Método que escreve o arquivo texto no caminho passado como parâmetro
    function SaveFileTXT_R(Arquivo: String): Boolean; // Método que escreve o arquivo texto no caminho passado como parâmetro
    function SaveFileTXT_T(Arquivo: String): Boolean; // Método que escreve o arquivo texto no caminho passado como parâmetro
    function SaveFileTXT_TITP(Arquivo: String): Boolean;
    function SaveFileTXT_RegistrosPAF(Arquivo: String): Boolean;

    property PAF_A: TPAF_A read FPAF_A write FPAF_A;
    property PAF_B: TPAF_B read FPAF_B write FPAF_B;
    property PAF_C: TPAF_C read FPAF_C write FPAF_C;
    property PAF_D: TPAF_D read FPAF_D write FPAF_D;
    property PAF_E: TPAF_E read FPAF_E write FPAF_E;
    property PAF_F: TPAF_F read FPAF_F write FPAF_F;
    property PAF_G: TPAF_G read FPAF_G write FPAF_G;
    property PAF_H: TPAF_H read FPAF_H write FPAF_H;
    property PAF_L: TPAF_L read FPAF_L write FPAF_L;
    property PAF_M: TPAF_M read FPAF_M write FPAF_M;
    property PAF_N: TPAF_N read FPAF_N write FPAF_N;
    property PAF_P: TPAF_P read FPAF_P write FPAF_P;
    property PAF_R: TPAF_R read FPAF_R write FPAF_R;
    property PAF_S: TPAF_S read FPAF_S write FPAF_S;
    property PAF_T: TPAF_T read FPAF_T write FPAF_T;
    property PAF_TITP: TPAF_TITP read FPAF_TITP write FPAF_TITP;
    property PAF_U: TPAF_U read FPAF_U write FPAF_U;

    Function GetACBrEAD : TACBrEAD ;
    function AssinaArquivoComEAD(Arquivo: String): Boolean;
  published
    property About : String   read GetAbout stored False ;
    property Path  : String   read FPath write FPath ;
    property EAD   : TACBrEAD read fsEAD write SetEAD ;
    property AAC   : TACBrAAC read fsAAC write SetAAC ;

    property Delimitador: String read GetDelimitador write SetDelimitador;
    property TrimString: Boolean read GetTrimString write SetTrimString
       default True ;
    property CurMascara: String read GetCurMascara write SetCurMascara;
    property AssinarArquivo : Boolean read FAssinar write FAssinar
      default True ;

    property OnError: TErrorEvent  read GetOnError write SetOnError;
    property OnPAFCalcEAD: TACBrEADCalc read fsOnPAFCalcEAD
       write fsOnPAFCalcEAD;
    property OnPAFGetKeyRSA: TACBrEADGetChave read fsOnPAFGetKeyRSA
       write fsOnPAFGetKeyRSA;
  end;

  procedure Register;

implementation

Uses
  {$IFDEF COMPILER6_UP} StrUtils {$ELSE} ACBrD5 {$ENDIF} ;

{$IFNDEF FPC}
 {$R ACBrPAF.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrPAF]);
end;

constructor TACBrPAF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPAF_A := TPAF_A.Create;
  FPAF_B := TPAF_B.Create;
  FPAF_C := TPAF_C.Create;
  FPAF_D := TPAF_D.Create;
  FPAF_E := TPAF_E.Create;
  FPAF_F := TPAF_F.Create;
  FPAF_G := TPAF_G.Create;
  FPAF_H := TPAF_H.Create;
  FPAF_L := TPAF_L.Create;
  FPAF_M := TPAF_M.Create;
  FPAF_N := TPAF_N.Create( Self );
  FPAF_P := TPAF_P.Create;
  FPAF_R := TPAF_R.Create;
  FPAF_T := TPAF_T.Create;
  FPAF_S := TPAF_S.Create;
  FPAF_TITP := TPAF_TITP.Create;
  FPAF_U := TPAF_U.Create;
  // Define o delimitador com o padrão PAF
  SetDelimitador('');
  // Define a mascara dos campos numéricos com o padrão PAF
  SetCurMascara('');

  fsEADInterno     := nil;
  fsEAD            := nil;
  fsOnPAFGetKeyRSA := nil;
  fsOnPAFCalcEAD   := nil;

  FPath := ExtractFilePath( ParamStr(0) );
  FDelimitador := '';
  FCurMascara  := '';
  FTrimString  := True;
  FAssinar     := True;
end;

destructor TACBrPAF.Destroy;
begin
  FPAF_A.Free;
  FPAF_B.Free;
  FPAF_C.Free;
  FPAF_D.Free;
  FPAF_E.Free;
  FPAF_F.Free;
  FPAF_G.Free;
  FPAF_H.Free;
  FPAF_L.Free;
  FPAF_M.Free;
  FPAF_N.Free;
  FPAF_P.Free;
  FPAF_R.Free;
  FPAF_S.Free;
  FPAF_T.Free;
  FPAF_TITP.Free;
  FPAF_U.Free;

  if Assigned( fsEADInterno ) then
     FreeAndNil( fsEADInterno );

  inherited;
end;

function TACBrPAF.GetAbout: String;
begin
  Result := 'ACBrPAF Ver: ' + CACBrPAF_Versao;
end;

function TACBrPAF.GetDelimitador: String;
begin
  Result := FDelimitador;
end;

procedure TACBrPAF.SetDelimitador(const Value: String);
begin
  FDelimitador := Value;

  FPAF_A.Delimitador := Value;
  FPAF_B.Delimitador := Value;
  FPAF_C.Delimitador := Value;
  FPAF_D.Delimitador := Value;
  FPAF_E.Delimitador := Value;
  FPAF_F.Delimitador := Value;
  FPAF_G.Delimitador := Value;
  FPAF_H.Delimitador := Value;
  FPAF_L.Delimitador := Value;
  FPAF_M.Delimitador := Value;
  FPAF_N.Delimitador := Value;
  FPAF_P.Delimitador := Value;
  FPAF_R.Delimitador := Value;
  FPAF_S.Delimitador := Value;
  FPAF_T.Delimitador := Value;
  FPAF_TITP.Delimitador := Value;
  FPAF_U.Delimitador := Value;
end;

function TACBrPAF.GetCurMascara: String;
begin
  Result := FCurMascara;
end;

procedure TACBrPAF.SetCurMascara(const Value: String);
begin
  FCurMascara := Value;

  FPAF_A.CurMascara := Value;
  FPAF_B.CurMascara := Value;
  FPAF_C.CurMascara := Value;
  FPAF_D.CurMascara := Value;
  FPAF_E.CurMascara := Value;
  FPAF_F.CurMascara := Value;
  FPAF_G.CurMascara := Value;
  FPAF_H.CurMascara := Value;
  FPAF_L.CurMascara := Value;
  FPAF_M.CurMascara := Value;
  FPAF_N.CurMascara := Value;
  FPAF_P.CurMascara := Value;
  FPAF_R.CurMascara := Value;
  FPAF_S.CurMascara := Value;
  FPAF_T.CurMascara := Value;
  FPAF_TITP.CurMascara := Value;
  FPAF_U.CurMascara := Value;
end;

function TACBrPAF.GetTrimString: boolean;
begin
  Result := FTrimString;
end;

procedure TACBrPAF.SetTrimString(const Value: boolean);
begin
  FTrimString := Value;

  FPAF_A.TrimString := Value;
  FPAF_B.TrimString := Value;
  FPAF_C.TrimString := Value;
  FPAF_D.TrimString := Value;
  FPAF_E.TrimString := Value;
  FPAF_F.TrimString := Value;
  FPAF_G.TrimString := Value;
  FPAF_H.TrimString := Value;
  FPAF_L.TrimString := Value;
  FPAF_M.TrimString := Value;
  FPAF_N.TrimString := Value;
  FPAF_P.TrimString := Value;
  FPAF_R.TrimString := Value;
  FPAF_S.TrimString := Value;
  FPAF_T.TrimString := Value;
  FPAF_TITP.TrimString := Value;
  FPAF_U.TrimString := Value;
end;

function TACBrPAF.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

procedure TACBrPAF.SetOnError(const Value: TErrorEvent);
begin
  FOnError := Value;

  FPAF_A.OnError := Value;
  FPAF_B.OnError := Value;
  FPAF_C.OnError := Value;
  FPAF_D.OnError := Value;
  FPAF_E.OnError := Value;
  FPAF_F.OnError := Value;
  FPAF_G.OnError := Value;
  FPAF_H.OnError := Value;
  FPAF_L.OnError := Value;
  FPAF_M.OnError := Value;
  FPAF_N.OnError := Value;
  FPAF_P.OnError := Value;
  FPAF_R.OnError := Value;
  FPAF_S.OnError := Value;
  FPAF_T.OnError := Value;
  FPAF_TITP.OnError := Value;
  FPAF_U.OnError := Value;
end;

procedure TACBrPAF.SetEAD(const AValue : TACBrEAD) ;
begin
  if AValue <> fsEAD then
  begin
     if Assigned(fsEAD) then
        fsEAD.RemoveFreeNotification(Self);

     fsEAD := AValue;

     if AValue <> nil then
     begin
        AValue.FreeNotification(self);

        if Assigned( fsEADInterno ) then
           FreeAndNil( fsEADInterno );
     end ;
  end ;
end ;

procedure TACBrPAF.SetAAC(const AValue : TACBrAAC) ;
begin
  if AValue <> fsAAC then
  begin
     if Assigned(fsAAC) then
        fsAAC.RemoveFreeNotification( Self );

     fsAAC := AValue;

     if AValue <> nil then
        AValue.FreeNotification(self);
  end ;
end ;

function TACBrPAF.SaveFileTXT_D(Arquivo: String): Boolean;
var
  txtFile: TextFile;
begin
  Result := True;

  if (Trim(Arquivo) = '') or (Trim(fPath) = '') then
    raise Exception.Create('Caminho ou nome do arquivo não informado!');

  try
    AssignFile(txtFile, fPath + Arquivo);
    try
      Rewrite(txtFile);
      Write(txtFile, FPAF_D.WriteRegistroD1);

      if FPAF_D.RegistroD2.Count > 0 then
         Write(txtFile, FPAF_D.WriteRegistroD2);

      Write(txtFile, FPAF_D.WriteRegistroD9);
    finally
      CloseFile(txtFile);
    end;

    // Assinatura EAD
    if FAssinar then
       AssinaArquivoComEAD(fPath + Arquivo);

    // Limpa de todos os Blocos as listas de todos os registros.
    FPAF_D.LimpaRegistros;
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;

function TACBrPAF.SaveFileTXT_E(Arquivo: String): Boolean;
var
  txtFile: TextFile;
begin
  Result := True;

  if (Trim(Arquivo) = '') or (Trim(fPath) = '') then
    raise Exception.Create('Caminho ou nome do arquivo não informado!');

  try
    AssignFile(txtFile, fPath + Arquivo);
    try
      Rewrite(txtFile);
      Write(txtFile, FPAF_E.WriteRegistroE1);

      if FPAF_E.RegistroE2.Count > 0 then
        Write(txtFile, FPAF_E.WriteRegistroE2);

      Write(txtFile, FPAF_E.WriteRegistroE9);
    finally
      CloseFile(txtFile);
    end;

    // Assinatura EAD
    if FAssinar then
      AssinaArquivoComEAD(fPath + Arquivo);

    // Limpa de todos os Blocos as listas de todos os registros.
    FPAF_E.LimpaRegistros;
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;


function TACBrPAF.SaveFileTXT_H(Arquivo: String): Boolean;
var
  txtFile: TextFile;
begin
  Result := True;

  if (Trim(Arquivo) = '') or (Trim(fPath) = '') then
    raise Exception.Create('Caminho ou nome do arquivo não informado!');

  try
    AssignFile(txtFile, fPath + Arquivo);
    try
      Rewrite(txtFile);
      Write(txtFile, FPAF_H.WriteRegistroH1);

      if FPAF_H.RegistroH2.Count > 0 then
        Write(txtFile, FPAF_H.WriteRegistroH2);

      Write(txtFile, FPAF_H.WriteRegistroH9);
    finally
      CloseFile(txtFile);
    end;

    // Assinatura EAD
    if FAssinar then
      AssinaArquivoComEAD(fPath + Arquivo);

    // Limpa de todos os Blocos as listas de todos os registros.
    FPAF_H.LimpaRegistros;
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;

function TACBrPAF.SaveFileTXT_P(Arquivo: String): Boolean;
var
txtFile: TextFile;
begin
  Result := True;

  if (Trim(Arquivo) = '') or (Trim(fPath) = '') then
    raise Exception.Create('Caminho ou nome do arquivo não informado!');

  try
    AssignFile(txtFile, fPath + Arquivo);
    try
      Rewrite(txtFile);

      Write(txtFile, FPAF_P.WriteRegistroP1);
      if FPAF_P.RegistroP2.Count > 0 then
        Write(txtFile, FPAF_P.WriteRegistroP2);

      Write(txtFile, FPAF_P.WriteRegistroP9);
    finally
      CloseFile(txtFile);
    end;

    // Assinatura EAD
    if FAssinar then
      AssinaArquivoComEAD(fPath + Arquivo);

    // Limpa de todos os Blocos as listas de todos os registros.
    FPAF_P.LimpaRegistros;
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;

function TACBrPAF.SaveFileTXT_R(Arquivo: String): Boolean;
var
txtFile: TextFile;
begin
  Result := True;

  if (Trim(Arquivo) = '') or (Trim(fPath) = '') then
    raise Exception.Create('Caminho ou nome do arquivo não informado!');

  try
    AssignFile(txtFile, fPath + Arquivo);
    try
      Rewrite(txtFile);

      Write(txtFile, FPAF_R.WriteRegistroR01);
     { if FPAF_R.RegistroR02.Count > 0 then
        Write(txtFile, FPAF_R.WriteRegistroR02);

      if FPAF_R.RegistroR04.Count > 0 then
        Write(txtFile, FPAF_R.WriteRegistroR04);

      if FPAF_R.RegistroR06.Count > 0 then
        Write(txtFile, FPAF_R.WriteRegistroR06);

      if (FPAF_R.RegistroR04.Count + FPAF_R.RegistroR06.Count) > 0 then
        Write(txtFile, FPAF_R.WriteRegistroR07);   }
    finally
      CloseFile(txtFile);
    end;

    // Assinatura EAD
    if FAssinar then
      AssinaArquivoComEAD(fPath + Arquivo);

    // Limpa de todos os Blocos as listas de todos os registros.
    FPAF_R.LimpaRegistros;
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;

function TACBrPAF.SaveFileTXT_T(Arquivo: String): Boolean;
var
txtFile: TextFile;
begin
  Result := True;

  if (Trim(Arquivo) = '') or (Trim(fPath) = '') then
    raise Exception.Create('Caminho ou nome do arquivo não informado!');

  try
    AssignFile(txtFile, fPath + Arquivo);
    try
      Rewrite(txtFile);
      Write(txtFile, FPAF_T.WriteRegistroT1);

      if FPAF_T.RegistroT2.Count > 0 then
        Write(txtFile, FPAF_T.WriteRegistroT2);

      Write(txtFile, FPAF_T.WriteRegistroT9);
    finally
      CloseFile(txtFile);
    end;

    // Assinatura EAD
    if FAssinar then
      AssinaArquivoComEAD(fPath + Arquivo);

    // Limpa de todos os Blocos as listas de todos os registros.
    FPAF_T.LimpaRegistros;
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;

function TACBrPAF.SaveFileTXT_TITP(Arquivo: String): Boolean;
var
  txtFile: TextFile;
begin
  Result := True;

  if (Trim(Arquivo) = '') or (Trim(fPath) = '') then
    raise Exception.Create('Caminho ou nome do arquivo não informado!');

  try
    AssignFile(txtFile, fPath + Arquivo);
    try
      Rewrite(txtFile);

      if Trim(FPAF_TITP.Titulo) <> '' then
      begin
        WriteLn(txtFile, FPAF_TITP.Titulo);
        WriteLn(txtFile, LinhaSimples(116));
      end;

      WriteLn(txtFile, FPAF_TITP.WriteMercadorias);

      WriteLn(txtFile, LinhaSimples(116));

      if FPAF_TITP.DataHora < 0 then
        FPAF_TITP.DataHora := NOW;

      WriteLn(txtFile, Format('Arquivo gerado em: %s %52d mercadoria(s) listada(s)', [
        FormatDateTime('dd/mm/yyyy "às" hh:mm', FPAF_TITP.DataHora),
        FPAF_TITP.Mercadorias.Count])
      );
    finally
      CloseFile(txtFile);
    end;

    // Assinatura EAD
    if FAssinar then
      AssinaArquivoComEAD(fPath + Arquivo);

    // Limpa de todos os Blocos as listas de todos os registros.
    FPAF_B.LimpaRegistros;
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;

function TACBrPAF.SaveFileTXT_B(Arquivo: string): Boolean;
var
txtFile: TextFile;
begin
  Result := True;

  if (Trim(Arquivo) = '') or (Trim(fPath) = '') then
    raise Exception.Create('Caminho ou nome do arquivo não informado!');

  try
    AssignFile(txtFile, fPath + Arquivo);
    try
      Rewrite(txtFile);

      Write(txtFile, FPAF_B.WriteRegistroB1);

      if FPAF_B.RegistroB2.Count > 0 then
        Write(txtFile, FPAF_B.WriteRegistroB2);

      Write(txtFile, FPAF_B.WriteRegistroB9);
    finally
      CloseFile(txtFile);
    end;

    // Assinatura EAD
    if FAssinar then
      AssinaArquivoComEAD(fPath + Arquivo);

    // Limpa de todos os Blocos as listas de todos os registros.
    FPAF_B.LimpaRegistros;
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;

function TACBrPAF.SaveFileTXT_C(Arquivo: String): Boolean;
var
txtFile: TextFile;
begin
  Result := True;

  if (Trim(Arquivo) = '') or (Trim(fPath) = '') then
    raise Exception.Create('Caminho ou nome do arquivo não informado!');

  try
    AssignFile(txtFile, fPath + Arquivo);
    try
      Rewrite(txtFile);

      Write(txtFile, FPAF_C.WriteRegistroC1);

      if FPAF_C.RegistroC2.Count > 0 then
        Write(txtFile, FPAF_C.WriteRegistroC2);

      Write(txtFile, FPAF_C.WriteRegistroC9);
    finally
      CloseFile(txtFile);
    end;

    // Assinatura EAD
    if FAssinar then
      AssinaArquivoComEAD(fPath + Arquivo);

    // Limpa de todos os Blocos as listas de todos os registros.
    FPAF_C.LimpaRegistros;
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;

function TACBrPAF.SaveFileTXT_N(Arquivo: String): Boolean;
var
  txtFile: TextFile;
  PAF_MD5 : String ;
  iFor: Integer;
begin
  Result := True;

  if (Trim(Arquivo) = '') or (Trim(fPath) = '') then
    raise Exception.Create('Caminho ou nome do arquivo não informado!');

  if Assigned( fsAAC ) then
  begin
     // Copie do AAC campos não informados do N1 //
    with FPAF_N.RegistroN1 do
    begin
      CNPJ        := ifthen( CNPJ = '', fsAAC.IdentPAF.Empresa.CNPJ, CNPJ ) ;
      IE          := ifthen( IE = '', fsAAC.IdentPAF.Empresa.IE, IE ) ;
      IM          := ifthen( IM = '', fsAAC.IdentPAF.Empresa.IM, IM ) ;
      RAZAOSOCIAL := ifthen( RAZAOSOCIAL = '', fsAAC.IdentPAF.Empresa.RazaoSocial,
                             RAZAOSOCIAL ) ;
    end;

    // Copie do AAC campos não informados do N2 //
    with FPAF_N.RegistroN2 do
    begin
      LAUDO  := ifthen( LAUDO = '', fsAAC.IdentPAF.NumeroLaudo, LAUDO ) ;
      NOME   := ifthen( NOME = '', fsAAC.IdentPAF.paf.Nome, NOME ) ;
      VERSAO := ifthen( VERSAO = '', fsAAC.IdentPAF.Paf.Versao, VERSAO ) ;
    end;

    // Se informou os arquivos no ACBrAAC copie-os para o N3 //
    if fsAAC.IdentPAF.OutrosArquivos.Count > 0 then
    begin
      FPAF_N.RegistroN3.Clear;
      For iFor := 0 to fsAAC.IdentPAF.OutrosArquivos.Count-1 do
      begin
        with FPAF_N.RegistroN3.New do
        begin
          NOME_ARQUIVO := fsAAC.IdentPAF.OutrosArquivos[iFor].Nome;
	      MD5 := '' ; // MD5 será calculado em WriteRegistroN3
        end ;
      end;
    end;
  end ;

  // Gravando arquivo N //
  AssignFile(txtFile, fPath + Arquivo);
  try
    Rewrite(txtFile);
    Write(txtFile, FPAF_N.WriteRegistroN1);
    Write(txtFile, FPAF_N.WriteRegistroN2);

    if FPAF_N.RegistroN3.Count > 0 then
      Write(txtFile, FPAF_N.WriteRegistroN3);

    Write(txtFile, FPAF_N.WriteRegistroN9);
  finally
    CloseFile(txtFile);
  end;

  // Assinatura EAD
  if FAssinar then
    AssinaArquivoComEAD(fPath + Arquivo);

  // Sincronizando arquivos e MD5 do ACBrPAF com ACBrAAC //
  if Assigned( fsAAC ) then
  begin
    AAC.IdentPAF.OutrosArquivos.Clear ;

    // Alimenta a lista de arquivos autenticados no AAC, para que essa lista
    // possa ser usada na impressão do relatório "Identificação do PAF-ECF"
    for iFor := 0 to FPAF_N.RegistroN3.Count - 1 do
    begin
      with AAC.IdentPAF.OutrosArquivos.New do
      begin
        Nome := FPAF_N.RegistroN3.Items[iFor].NOME_ARQUIVO;
        MD5  := FPAF_N.RegistroN3.Items[iFor].MD5;
      end;
    end;

    // Gera o MD5 do arquivo
    PAF_MD5 := GetACBrEAD.MD5FromFile( fPath + Arquivo );

    // Informações do arquivo com a lista de arquivos autenticados
    AAC.IdentPAF.ArquivoListaAutenticados.Nome := Arquivo;
    // Atualiza AAC.IdentPAF.ArquivoListaAutenticados.MD5
    AAC.AtualizarMD5( PAF_MD5 );
  end ;

  // Limpa de todos os Blocos as listas de todos os registros.
  FPAF_N.LimpaRegistros;
end;

procedure TACBrPAF.Notification(AComponent : TComponent ; Operation : TOperation
   ) ;
begin
   inherited Notification(AComponent, Operation) ;

  if (Operation = opRemove) and (AComponent is TACBrEAD) and (fsEAD <> nil) then
     fsEAD := nil ;

  if (Operation = opRemove) and (AComponent is TACBrAAC) and (fsAAC <> nil) then
     fsAAC := nil ;
end ;

function TACBrPAF.GetACBrEAD : TACBrEAD ;
begin
  if Assigned(fsEAD) then
     Result := fsEAD
  else
   begin
     if not Assigned( fsEADInterno ) then
     begin
        fsEADInterno := TACBrEAD.Create(Self);
        fsEADInterno.OnGetChavePrivada := fsOnPAFGetKeyRSA;
     end ;
     Result := fsEADInterno;
   end ;
end ;

function TACBrPAF.AssinaArquivoComEAD(Arquivo: String): Boolean;
begin
  if Assigned( fsOnPAFCalcEAD ) then
     fsOnPAFCalcEAD( Arquivo )
  else
     GetACBrEAD.AssinarArquivoComEAD( Arquivo ) ;

  Result := True;
end;

function TACBrPAF.SaveFileTXT_RegistrosPAF(Arquivo: String): Boolean;
var
  txtFile: TextFile;
begin
  Result := True;

  if (Trim(Arquivo) = '') or (Trim(fPath) = '') then
    raise Exception.Create('Caminho ou nome do arquivo não informado!');

  try
    AssignFile(txtFile, fPath + Arquivo);
    try
      Rewrite(txtFile);

      Write(txtFile, PAF_U.WriteRegistroU1);
      if FPAF_A.RegistroA2.Count > 0 then
        Write(txtFile, FPAF_A.WriteRegistroA2);

      if FPAF_P.RegistroP2.Count > 0 then
        Write(txtFile, FPAF_P.WriteRegistroP2);

      if FPAF_E.RegistroE2.Count > 0 then
        begin
          Write(txtFile, FPAF_E.WriteRegistroE2);
          Write(txtFile, FPAF_E.WriteRegistroE3);
        end;

      if FPAF_D.RegistroD2.Count > 0 then
        Write(txtFile, FPAF_D.WriteRegistroD2);  //D2 e D3

      //if FPAF_D.RegistroD4.Count > 0 then
      //  Write(txtFile, FPAF_D.WriteRegistroD4);

      if FPAF_B.RegistroB2.Count > 0 then
        Write(txtFile, FPAF_B.WriteRegistroB2);

      if FPAF_C.RegistroC2.Count > 0 then
        Write(txtFile, FPAF_C.WriteRegistroC2);

      if FPAF_F.RegistroF2.Count > 0 then
        Write(txtFile, FPAF_F.WriteRegistroF2);

      if FPAF_F.RegistroF3.Count > 0 then
        Write(txtFile, FPAF_F.WriteRegistroF3);

      if FPAF_F.RegistroF4.Count > 0 then
        Write(txtFile, FPAF_F.WriteRegistroF4);

      if FPAF_T.RegistroT2.Count > 0 then
        Write(txtFile, FPAF_T.WriteRegistroT2);

      if FPAF_M.RegistroM2.Count > 0 then
        Write(txtFile, FPAF_M.WriteRegistroM2);

      if FPAF_L.RegistroL2.Count > 0 then
        Write(txtFile, FPAF_L.WriteRegistroL2);

      if FPAF_G.RegistroG2.Count > 0 then
        Write(txtFile, FPAF_G.WriteRegistroG2);
        
      if FPAF_H.RegistroH2.Count > 0 then
        Write(txtFile, FPAF_H.WriteRegistroH2);

      if FPAF_S.RegistroS2.Count > 0 then        //S2 e S3
        Write(txtFile, FPAF_S.WriteRegistroS2);

      Write(txtFile, FPAF_R.WriteRegistroR01);
     { if FPAF_R.RegistroR02.Count > 0 then
        Write(txtFile, FPAF_R.WriteRegistroR02);

      if FPAF_R.RegistroR04.Count > 0 then
        Write(txtFile, FPAF_R.WriteRegistroR04); //R04 e R05

      if FPAF_R.RegistroR06.Count > 0 then
        Write(txtFile, FPAF_R.WriteRegistroR06);

      if (FPAF_R.RegistroR04.Count + FPAF_R.RegistroR06.Count) > 0 then
        Write(txtFile, FPAF_R.WriteRegistroR07); }

    finally
      CloseFile(txtFile);
    end;

    // Assinatura EAD
    if FAssinar then
      AssinaArquivoComEAD(fPath + Arquivo);

    //Limpa de todos os Blocos as listas de todos os registros.
    FPAF_U.LimpaRegistros;
    FPAF_A.LimpaRegistros;
    FPAF_P.LimpaRegistros;
    FPAF_E.LimpaRegistros;
    FPAF_D.LimpaRegistros;
    FPAF_B.LimpaRegistros;
    FPAF_C.LimpaRegistros;
    FPAF_F.LimpaRegistros;
    FPAF_T.LimpaRegistros;
    FPAF_M.LimpaRegistros;
    FPAF_L.LimpaRegistros;
    FPAF_G.LimpaRegistros;
    FPAF_H.LimpaRegistros;
    FPAF_S.LimpaRegistros;
    FPAF_R.LimpaRegistros;
  except
    on E: Exception do
      begin
        Result:= False;
        raise Exception.Create(E.Message);
      end;
  end;
end;

{$IFNDEF NOGUI}
{$IFDEF FPC}
initialization
   {$I ACBrPAF.lrs}
{$ENDIF}
{$ENDIF}

end.

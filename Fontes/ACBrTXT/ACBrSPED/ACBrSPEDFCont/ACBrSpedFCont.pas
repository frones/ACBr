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
*******************************************************************************}

unit ACBrSpedFCont;

interface

uses
  SysUtils, Classes, DateUtils, ACBrBase,
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
  ACBrSped, ACBrTXTClass,
  ACBrFContBloco_0_Class, ACBrFContBloco_9_Class, ACBrFContBloco_I_Class,
  ACBrFContBloco_J_Class, ACBrFContBloco_M_Class;

type
  /// ACBrSpedFCont -  Controle Fiscal Contábil de Transição

  { TACBrSPEDFCont }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrSPEDFCont = class(TACBrComponent)
  private
    FACBrTXT: TACBrTXTClass;
    FOnError: TErrorEvent;

    FDT_INI: TDateTime;           /// Data inicial das informações contidas no arquivo
    FDT_FIN: TDateTime;           /// Data final das informações contidas no arquivo

    FPath: String;            /// Path do arquivo a ser gerado
    FDelimitador: String;     /// Caracter delimitador de campos
    FTrimString: boolean;         /// Retorna a string sem espaços em branco iniciais e finais
    FCurMascara: String;      /// Mascara para valores tipo currency

    FBloco_0: TBloco_0;
    FBloco_9: TBloco_9;
    FBloco_I: TBloco_I;
    FBloco_J: TBloco_J;
    FBloco_M: TBloco_M;    

    function GetDelimitador: String;
    function GetTrimString: boolean;
    function GetCurMascara: String;
    function GetDT_FIN: TDateTime;
    function GetDT_INI: TDateTime;
    procedure SetDelimitador(const Value: String);
    procedure SetTrimString(const Value: boolean);
    procedure SetCurMascara(const Value: String);
    procedure SetDT_FIN(const Value: TDateTime);
    procedure SetDT_INI(const Value: TDateTime);

    function GetOnError: TErrorEvent; /// Método do evento OnError
    procedure SetOnError(const Value: TErrorEvent); /// Método SetError

    procedure LimpaRegistros;
  protected
    /// BLOCO 0
    function WriteRegistro0000: String;
    /// BLOCO I
    function WriteRegistroI001: String;
    function WriteRegistroI050: String;
    function WriteRegistroI075: String;
    function WriteRegistroI100: String;
    function WriteRegistroI150: String;
    function WriteRegistroI200: String;
    function WriteRegistroI350: String;

    function WriteRegistroI990: String;
    /// BLOCO J
    function WriteRegistroJ001: String;
    function WriteRegistroJ930: String;
    function WriteRegistroJ990: String;

    /// BLOCO M
    function WriteRegistroM001: String;
    function WriteRegistroM020: String;
    function WriteRegistroM025: String;
    function WriteRegistroM030: String;
    function WriteRegistroM990: String;

    /// BLOCO 9
    function WriteRegistro9001: String;
    function WriteRegistro9900: String;
    function WriteRegistro9990: String;
    function WriteRegistro9999: String;
  public
    constructor Create(AOwner: TComponent); override; /// Create
    destructor Destroy; override; /// Destroy

    function SaveFileTXT(const Arquivo: String): Boolean; /// Método que escreve o arquivo texto no caminho passado como parâmetro
    function SaveStringList(AStringList: TStringList): Boolean; /// Método que escreve o arquivo texto em um StringList

    property DT_INI: TDateTime read GetDT_INI write SetDT_INI;
    property DT_FIN: TDateTime read GetDT_FIN write SetDT_FIN;
    //
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property Bloco_9: TBloco_9 read FBloco_9 write FBloco_9;
    property Bloco_I: TBloco_I read FBloco_I write FBloco_I;
    property Bloco_J: TBloco_J read FBloco_J write FBloco_J;
    property Bloco_M: TBloco_M read FBloco_M write FBloco_M;
  published
    property Path: String read FPath write FPath;
    ///
    property Delimitador: String read GetDelimitador write SetDelimitador;
    property TrimString: boolean read GetTrimString write SetTrimString;
    property CurMascara: String read GetCurMascara write SetCurMascara;

    property OnError: TErrorEvent  read GetOnError write SetOnError;
  end;

procedure Register;

implementation

Uses ACBrUtil ;

{$IFNDEF FPC}
 {$R ACBr_SPEDFCont.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrSPEDFCont]);
end;

(* TACBrSPEDFCont *)

constructor TACBrSPEDFCont.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FACBrTXT := TACBrTXTClass.Create;

  FBloco_0 := TBloco_0.Create;
  FBloco_I := TBloco_I.Create;
  FBloco_J := TBloco_J.Create;
  FBloco_M := TBloco_M.Create;
  FBloco_9 := TBloco_9.Create;

  FPath := ExtractFilePath( ParamStr(0) );
  FDelimitador := '|';
  FCurMascara  := '#0.00';
  FTrimString  := True;
end;

destructor TACBrSPEDFCont.Destroy;
begin
  FACBrTXT.Free;

  FBloco_0.Free;
  FBloco_I.Free;
  FBloco_J.Free;
  FBloco_M.Free;
  FBloco_9.Free;
  inherited;
end;

procedure TACBrSPEDFCont.LimpaRegistros;
begin
  FBloco_I.LimpaRegistros;
  FBloco_J.LimpaRegistros;
  FBloco_M.LimpaRegistros;
  FBloco_9.LimpaRegistros;
end;

function TACBrSPEDFCont.GetDelimitador: String;
begin
   Result := FDelimitador;
end;

procedure TACBrSPEDFCont.SetDelimitador(const Value: String);
begin
  FDelimitador := Value;
  //
  FBloco_0.Delimitador := Value;
  FBloco_I.Delimitador := Value;
  FBloco_J.Delimitador := Value;
  FBloco_M.Delimitador := Value;
  FBloco_9.Delimitador := Value;
end;

function TACBrSPEDFCont.GetCurMascara: String;
begin
   Result := FCurMascara;
end;

procedure TACBrSPEDFCont.SetCurMascara(const Value: String);
begin
  FCurMascara := Value;
  //
  FBloco_0.CurMascara := Value;
  FBloco_I.CurMascara := Value;
  FBloco_J.CurMascara := Value;
  FBloco_M.CurMascara := Value;
  FBloco_9.CurMascara := Value;
end;

function TACBrSPEDFCont.GetTrimString: boolean;
begin
   Result := FTrimString;
end;

procedure TACBrSPEDFCont.SetTrimString(const Value: boolean);
begin
  FTrimString := Value;
  //
  FBloco_0.TrimString := Value;
  FBloco_I.TrimString := Value;
  FBloco_J.TrimString := Value;
  FBloco_M.TrimString := Value;
  FBloco_9.TrimString := Value;
end;

function TACBrSPEDFCont.GetDT_INI: TDateTime;
begin
   Result := FDT_INI;
end;

procedure TACBrSPEDFCont.SetDT_INI(const Value: TDateTime);
begin
  FDT_INI := Value;
  //
  FBloco_0.DT_INI := Value;
  FBloco_I.DT_INI := Value;
  FBloco_J.DT_INI := Value;
  FBloco_M.DT_INI := Value;
  FBloco_9.DT_INI := Value;
  //
  if Assigned(FBloco_0) then
  begin
     FBloco_0.Registro0000.DT_INI := Value;
  end;
end;

function TACBrSPEDFCont.GetDT_FIN: TDateTime;
begin
   Result := FDT_FIN;
end;

procedure TACBrSPEDFCont.SetDT_FIN(const Value: TDateTime);
begin
  FDT_FIN := Value;
  //
  FBloco_0.DT_FIN := Value;
  FBloco_I.DT_FIN := Value;
  FBloco_J.DT_FIN := Value;
  FBloco_M.DT_FIN := Value;
  FBloco_9.DT_FIN := Value;
  //
  if Assigned(FBloco_0) then
  begin
     FBloco_0.Registro0000.DT_FIN := Value;
  end;
end;

function TACBrSPEDFCont.GetOnError: TErrorEvent;
begin
   Result := FOnError;
end;

procedure TACBrSPEDFCont.SetOnError(const Value: TErrorEvent);
begin
  FOnError := Value;

  FBloco_0.OnError := Value;
  FBloco_I.OnError := Value;
  FBloco_J.OnError := Value;
  FBloco_M.OnError := Value;
  FBloco_9.OnError := Value;
end;

function TACBrSPEDFCont.SaveFileTXT(const Arquivo: String): Boolean;
var
   SL : TStringList ;
begin
  if FDT_INI = 0 then
     raise Exception.Create( ACBrStr('Informe a data inicial das informações contidas no arquivo!'));

  if FDT_FIN = 0 then
     raise Exception.Create( ACBrStr('Informe a data final das informações contidas no arquivo!'));

  if (Trim(Arquivo) = '') or (Trim(fPath) = '') then
     raise Exception.Create( ACBrStr('Caminho ou nome do arquivo não informado!'));

  SL := TStringList.Create;
  try
     SaveStringList( SL );
     SL.SaveToFile( fPath + Arquivo );
     Result := True ;
  finally
     SL.Free;
  end;
end;

function TACBrSPEDFCont.SaveStringList(AStringList: TStringList): Boolean;
var
  intFor: integer;
begin
  Result := True;

//  Check(DT_INI > 0,        'CHECAGEM INICIAL: Informe a data inicial das informações contidas no arquivo!');
//  Check(DT_FIN > 0,        'CHECAGEM INICIAL: Informe a data final das informações contidas no arquivo!');
//  Check(DayOf(DT_INI) = 1, 'CHECAGEM INICIAL: A data inicial deve corresponder ao primeiro dia do mês informado!');
//  Check(DT_FIN >= DT_INI,  'CHECAGEM INICIAL: A data final deve se maior que a data inicial!');
//  Check(DT_FIN <= Date,    'CHECAGEM INICIAL: A data final "%s" não pode ser superior a data atual "%s"!', [DateToStr(DT_FIN), DateToStr(Date)]);
//  Check(DateOf(EndOfTheMonth(DT_FIN)) = DateOf(DT_FIN), 'CHECAGEM INICIAL: A data final deve corresponder ao último dia do mês informado!');

  /// Preparação para totalizações de registros.
  Bloco_I.RegistroI990.QTD_LIN_I := 0;
  Bloco_J.RegistroJ990.QTD_LIN_J := 0;
  Bloco_M.RegistroM990.QTD_LIN_M := 0;
  Bloco_9.Registro9990.QTD_LIN_9 := 0;
  Bloco_9.Registro9999.QTD_LIN   := 0;
  ///
  for intFor := 0 to Bloco_9.Registro9900.Count - 1 do
  begin
     Bloco_9.Registro9900.Items[intFor].Free;
  end;
  Bloco_9.Registro9900.Clear;

  try
    /// BLOCO 0
    AStringList.Add(Trim(WriteRegistro0000));

    /// BLOCO I
    AStringList.Add(Trim(WriteRegistroI001));
    if Bloco_I.RegistroI050.Count > 0 then AStringList.Add(Trim(WriteRegistroI050));
    if Bloco_I.RegistroI075.Count > 0 then AStringList.Add(Trim(WriteRegistroI075));
    if Bloco_I.RegistroI100.Count > 0 then AStringList.Add(Trim(WriteRegistroI100));
    if Bloco_I.RegistroI150.Count > 0 then AStringList.Add(Trim(WriteRegistroI150));
    if Bloco_I.RegistroI200.Count > 0 then AStringList.Add(Trim(WriteRegistroI200));
    if Bloco_I.RegistroI350.Count > 0 then AStringList.Add(Trim(WriteRegistroI350));

    AStringList.Add(Trim(WriteRegistroI990));

   /// BLOCO J
    AStringList.Add(Trim(WriteRegistroJ001));
    if Bloco_J.RegistroJ930.Count > 0 then AStringList.Add(Trim(WriteRegistroJ930));
    AStringList.Add(Trim(WriteRegistroJ990));

   /// BLOCO M
    AStringList.Add(Trim(WriteRegistroM001));
    if Bloco_M.RegistroM001.IND_DAD = 0 then AStringList.Add(Trim(WriteRegistroM020));
    if Bloco_M.RegistroM025.Count > 0 then AStringList.Add(Trim(WriteRegistroM025));
    if Bloco_M.RegistroM030.Count > 0 then AStringList.Add(Trim(WriteRegistroM030));
    AStringList.Add(Trim(WriteRegistroM990));

    /// BLOCO 9
    AStringList.Add(Trim(WriteRegistro9001));
    AStringList.Add(Trim(WriteRegistro9900));
    AStringList.Add(Trim(WriteRegistro9990));
    AStringList.Add(Trim(WriteRegistro9999));

  finally
    /// Limpa de todos os Blocos as listas de todos os registros.
    LimpaRegistros;
  end;
end;

function TACBrSPEDFCont.WriteRegistro0000: String;
begin
   Result := Bloco_0.WriteRegistro0000;

   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '0000';
      QTD_REG_BLC := 1;
   end;
end;

function TACBrSPEDFCont.WriteRegistroI001: String;
begin
   Result := Bloco_I.WriteRegistroI001;

   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'I001';
      QTD_REG_BLC := 1;
   end;
end;

function TACBrSPEDFCont.WriteRegistroI050: String;
begin
   Result := Bloco_I.WriteRegistroI050;

   if Bloco_I.RegistroI050.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I050';
         QTD_REG_BLC := Bloco_I.RegistroI050.Count;
      end;
   end;
   if Bloco_I.RegistroI051Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I051';
         QTD_REG_BLC := Bloco_I.RegistroI051Count;
      end;
   end;
end;

function TACBrSPEDFCont.WriteRegistroI075: String;
begin
   Result := Bloco_I.WriteRegistroI075;

   if Bloco_I.RegistroI075.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I075';
         QTD_REG_BLC := Bloco_I.RegistroI075.Count;
      end;
   end;
end;

function TACBrSPEDFCont.WriteRegistroI100: String;
begin
   Result := Bloco_I.WriteRegistroI100;

   if Bloco_I.RegistroI100.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I100';
         QTD_REG_BLC := Bloco_I.RegistroI100.Count;
      end;
   end;
end;

function TACBrSPEDFCont.WriteRegistroI150: String;
begin
   Result := Bloco_I.WriteRegistroI150;

   if Bloco_I.RegistroI150.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I150';
         QTD_REG_BLC := Bloco_I.RegistroI150.Count;
      end;
   end;
   if Bloco_I.RegistroI155Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I155';
         QTD_REG_BLC := Bloco_I.RegistroI155Count;
      end;
   end;
   if Bloco_I.RegistroI156Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I156';
         QTD_REG_BLC := Bloco_I.RegistroI156Count;
      end;
   end;
end;

function TACBrSPEDFCont.WriteRegistroI200: String;
begin
   Result := Bloco_I.WriteRegistroI200;

   if Bloco_I.RegistroI200.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I200';
         QTD_REG_BLC := Bloco_I.RegistroI200.Count;
      end;
   end;
   if Bloco_I.RegistroI250Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I250';
         QTD_REG_BLC := Bloco_I.RegistroI250Count;
      end;
   end;
   if Bloco_I.RegistroI256Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I256';
         QTD_REG_BLC := Bloco_I.RegistroI256Count;
      end;
   end;
end;

function TACBrSPEDFCont.WriteRegistroI350: String;
begin
   Result := Bloco_I.WriteRegistroI350;

   if Bloco_I.RegistroI350.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I350';
         QTD_REG_BLC := Bloco_I.RegistroI350.Count;
      end;
   end;
   if Bloco_I.RegistroI355Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I355';
         QTD_REG_BLC := Bloco_I.RegistroI355Count;
      end;
   end;
   if Bloco_I.RegistroI356Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I356';
         QTD_REG_BLC := Bloco_I.RegistroI356Count;
      end;
   end;
end;

function TACBrSPEDFCont.WriteRegistroI990: String;
begin
   Result := Bloco_I.WriteRegistroI990;

   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'I990';
      QTD_REG_BLC := 1;
   end;
end;

function TACBrSPEDFCont.WriteRegistroJ001: String;
begin
   Result := Bloco_J.WriteRegistroJ001;

   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'J001';
      QTD_REG_BLC := 1;
   end;
end;

function TACBrSPEDFCont.WriteRegistroJ930: String;
begin
   Result := Bloco_J.WriteRegistroJ930;

   if Bloco_J.RegistroJ930.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'J930';
         QTD_REG_BLC := Bloco_J.RegistroJ930.Count;
      end;
   end;
end;

function TACBrSPEDFCont.WriteRegistroJ990: String;
begin
   Result := Bloco_J.WriteRegistroJ990;

   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'J990';
      QTD_REG_BLC := 1;
   end;
end;

function TACBrSPEDFCont.WriteRegistroM001: String;
begin
   Result := Bloco_M.WriteRegistroM001;

   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'M001';
      QTD_REG_BLC := 1;
   end;
end;

function TACBrSPEDFCont.WriteRegistroM020: String;
begin
   Result := Bloco_M.WriteRegistroM020;

   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'M020';
      QTD_REG_BLC := 1;
   end;
end;

function TACBrSPEDFCont.WriteRegistroM025: String;
begin
   Result := Bloco_M.WriteRegistroM025;

   if Bloco_M.RegistroM025.Count > 0 then
   begin
     with Bloco_9.Registro9900.New do
     begin
       REG_BLC := 'M025';
       QTD_REG_BLC := Bloco_M.RegistroM025.Count;
     end;
   end;
end;


function TACBrSPEDFCont.WriteRegistroM030: String;
begin
   Result := Bloco_M.WriteRegistroM030;

   if Bloco_M.RegistroM030.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'M030';
         QTD_REG_BLC := Bloco_M.RegistroM030.Count;
      end;
   end;
   if Bloco_M.RegistroM155Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'M155';
         QTD_REG_BLC := Bloco_M.RegistroM155Count;
      end;
   end;
   if Bloco_M.RegistroM355Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'M355';
         QTD_REG_BLC := Bloco_M.RegistroM355Count;
      end;
   end;
end;

function TACBrSPEDFCont.WriteRegistroM990: String;
begin
   Result := Bloco_M.WriteRegistroM990;

   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'M990';
      QTD_REG_BLC := 1;
   end;
end;


function TACBrSPEDFCont.WriteRegistro9001: String;
begin
   Result := Bloco_9.WriteRegistro9001;

   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '9001';
      QTD_REG_BLC := 1;
   end;
end;

function TACBrSPEDFCont.WriteRegistro9900: String;
begin
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := '9900';
         QTD_REG_BLC := Bloco_9.Registro9900.Count + 2;
      end;
      with New do
      begin
         REG_BLC := '9990';
         QTD_REG_BLC := 1;
      end;
      with New do
      begin
         REG_BLC := '9999';
         QTD_REG_BLC := 1;
      end;
   end;
   Result := Bloco_9.WriteRegistro9900;
end;

function TACBrSPEDFCont.WriteRegistro9990: String;
begin
   Result := Bloco_9.WriteRegistro9990;
end;

function TACBrSPEDFCont.WriteRegistro9999: String;
begin
   Bloco_9.Registro9999.QTD_LIN := Bloco_9.Registro9999.QTD_LIN + 1 +
                                                                  Bloco_I.RegistroI990.QTD_LIN_I +
                                                                  Bloco_J.RegistroJ990.QTD_LIN_J +
                                                                  Bloco_M.RegistroM990.QTD_LIN_M +
                                                                  Bloco_9.Registro9990.QTD_LIN_9;
   Result := Bloco_9.WriteRegistro9999;
end;

{$ifdef FPC}

initialization
   {$I ACBrSpedFCont.lrs}
{$endif}

end.

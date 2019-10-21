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
|* 04/03/2015: Flavio Rubens Massaro Jr.
|* - Modificação para contemplar layout 3 referente ao ano calendario 2014
|* 16/05/2017: Renato Rubinho
|*  - Criação e distribuição para o layout 5
*******************************************************************************}

unit ACBrSpedContabil;

interface

uses
  SysUtils, Classes, DateUtils, ACBrBase,
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
  ACBrSped, ACBrTXTClass,
  ACBrECDBloco_0_Class, ACBrECDBloco_9_Class, ACBrECDBloco_I_Class,
  ACBrECDBloco_J_Class, ACBrECDBloco_K_Class;

type

  /// ACBrSpedContabil - Sitema Publico de Escrituração Digital Contabil

  { TACBrSPEDContabil }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrSPEDContabil = class(TACBrComponent)
  private
    FACBrTXT: TACBrTXTClass;
    FArquivo: String;
    FInicializado : boolean;
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
    FBloco_K: TBloco_K;

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
    function GetLinhasBuffer: Integer;
    procedure SetLinhasBuffer(const Value: Integer);

    procedure TotalizarTermos;

    procedure InicializaBloco(Bloco: TACBrSPED);
    procedure SetArquivo(const Value: String);
    function GetConteudo: TStringList;
  protected
    /// BLOCO 0
    procedure WriteRegistro0000;
    procedure WriteRegistro0001;
    procedure WriteRegistro0007;
    procedure WriteRegistro0020;
    procedure WriteRegistro0035;
    procedure WriteRegistro0150;
    //procedure WriteRegistro0180;
    procedure WriteRegistro0990;
    /// BLOCO I
    procedure WriteRegistroI001;
    procedure WriteRegistroI010;
    procedure WriteRegistroI012;
    procedure WriteRegistroI020;
    procedure WriteRegistroI030;
    procedure WriteRegistroI050;
    procedure WriteRegistroI075;
    procedure WriteRegistroI100;
    procedure WriteRegistroI150;
    procedure WriteRegistroI200;
    procedure WriteRegistroI300;
    procedure WriteRegistroI350;
    procedure WriteRegistroI500;
    procedure WriteRegistroI510;
    procedure WriteRegistroI550;

    procedure WriteRegistroI990;
    /// BLOCO J
    procedure WriteRegistroJ001;
    procedure WriteRegistroJ005;
//  procedure WriteRegistroJ800;
//  procedure WriteRegistroJ801;
    procedure WriteRegistroJ900;
    procedure WriteRegistroJ930;
    procedure WriteRegistroJ932;
  	procedure WriteRegistroJ935;
    procedure WriteRegistroJ990;
    /// BLOCO K
    procedure WriteRegistroK001;
    procedure WriteRegistroK030;
    procedure WriteRegistroK990;
    /// BLOCO 9
    procedure WriteRegistro9001;
    procedure WriteRegistro9900;
    procedure WriteRegistro9990;
    procedure WriteRegistro9999;
  public
    constructor Create(AOwner: TComponent); override; /// Create
    destructor Destroy; override; /// Destroy

    procedure SaveFileTXT; /// Método que escreve o arquivo texto no caminho passado como parâmetro

    procedure IniciaGeracao;

    procedure WriteBloco_0;
    procedure WriteBloco_I;
    procedure WriteBloco_J;
    procedure WriteBloco_K;
    procedure WriteBloco_9;


    property Conteudo: TStringList read GetConteudo;

    property DT_INI: TDateTime read GetDT_INI write SetDT_INI;
    property DT_FIN: TDateTime read GetDT_FIN write SetDT_FIN;
    //
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property Bloco_9: TBloco_9 read FBloco_9 write FBloco_9;
    property Bloco_I: TBloco_I read FBloco_I write FBloco_I;
    property Bloco_J: TBloco_J read FBloco_J write FBloco_J;
    property Bloco_K: TBloco_K read FBloco_K write FBloco_K;
  published
    property Path: String read FPath write FPath;
    ///
    property Delimitador: String read GetDelimitador write SetDelimitador;
    property TrimString: boolean read GetTrimString write SetTrimString;
    property CurMascara: String read GetCurMascara write SetCurMascara;

    property Arquivo: String read FArquivo write SetArquivo;

    property OnError: TErrorEvent  read GetOnError write SetOnError;

    property LinhasBuffer : Integer read GetLinhasBuffer write SetLinhasBuffer
      default 1000 ;
  end;

procedure Register;

implementation

Uses ACBrUtil, ACBrECDBloco_9, ACBrECDBloco_I, ACBrECDBloco_K ;

{$IFNDEF FPC}
 {$R ACBr_SPEDContabil.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrSPEDContabil]);
end;

(* TACBrSPEDContabil *)

constructor TACBrSPEDContabil.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FACBrTXT := TACBrTXTClass.Create;
  FACBrTXT.LinhasBuffer := 1000 ;

  FInicializado := False;

  FBloco_0 := TBloco_0.Create;
  FBloco_I := TBloco_I.Create;
  FBloco_J := TBloco_J.Create;
  FBloco_K := TBloco_K.Create;
  FBloco_9 := TBloco_9.Create;

  FBloco_I.Bloco_0 := FBloco_0;
  FBloco_J.Bloco_0 := FBloco_0;
  FBloco_K.Bloco_0 := FBloco_0;
  FBloco_9.Bloco_0 := FBloco_0;


  FPath := ExtractFilePath( ParamStr(0) );
  FDelimitador := '|';
  FCurMascara  := '#0.00';
  FTrimString  := True;
end;

destructor TACBrSPEDContabil.Destroy;
begin
  FACBrTXT.Free;

  FBloco_0.Free;
  FBloco_I.Free;
  FBloco_J.Free;
  FBloco_K.Free;
  FBloco_9.Free;
  inherited;
end;

procedure TACBrSPEDContabil.LimpaRegistros;
begin
  FBloco_0.LimpaRegistros;
  FBloco_I.LimpaRegistros;
  FBloco_J.LimpaRegistros;
  FBloco_K.LimpaRegistros;
  FBloco_9.LimpaRegistros;
end;

function TACBrSPEDContabil.GetDelimitador: String;
begin
   Result := FDelimitador;
end;

procedure TACBrSPEDContabil.SetDelimitador(const Value: String);
begin
  FDelimitador := Value;
  //
  FBloco_0.Delimitador := Value;
  FBloco_I.Delimitador := Value;
  FBloco_J.Delimitador := Value;
  FBloco_K.Delimitador := Value;
  FBloco_9.Delimitador := Value;
end;

function TACBrSPEDContabil.GetCurMascara: String;
begin
   Result := FCurMascara;
end;

procedure TACBrSPEDContabil.SetCurMascara(const Value: String);
begin
  FCurMascara := Value;
  //
  FBloco_0.CurMascara := Value;
  FBloco_I.CurMascara := Value;
  FBloco_J.CurMascara := Value;
  FBloco_K.CurMascara := Value;
  FBloco_9.CurMascara := Value;
end;

function TACBrSPEDContabil.GetTrimString: boolean;
begin
   Result := FTrimString;
end;

procedure TACBrSPEDContabil.SetTrimString(const Value: boolean);
begin
  FTrimString := Value;
  //
  FBloco_0.TrimString := Value;
  FBloco_I.TrimString := Value;
  FBloco_J.TrimString := Value;
  FBloco_K.TrimString := Value;
  FBloco_9.TrimString := Value;
end;

function TACBrSPEDContabil.GetDT_INI: TDateTime;
begin
   Result := FDT_INI;
end;

procedure TACBrSPEDContabil.SetDT_INI(const Value: TDateTime);
begin
  FDT_INI := Value;
  //
  FBloco_0.DT_INI := Value;
  FBloco_I.DT_INI := Value;
  FBloco_J.DT_INI := Value;
  FBloco_K.DT_INI := Value;
  FBloco_9.DT_INI := Value;
  //
  if Assigned(FBloco_0) then
  begin
     FBloco_0.Registro0000.DT_INI := Value;
  end;
end;

function TACBrSPEDContabil.GetDT_FIN: TDateTime;
begin
   Result := FDT_FIN;
end;

procedure TACBrSPEDContabil.SetDT_FIN(const Value: TDateTime);
begin
  FDT_FIN := Value;
  //
  FBloco_0.DT_FIN := Value;
  FBloco_I.DT_FIN := Value;
  FBloco_J.DT_FIN := Value;
  FBloco_K.DT_FIN := Value;
  FBloco_9.DT_FIN := Value;
  //
  if Assigned(FBloco_0) then
  begin
     FBloco_0.Registro0000.DT_FIN := Value;
  end;
end;

function TACBrSPEDContabil.GetOnError: TErrorEvent;
begin
   Result := FOnError;
end;

procedure TACBrSPEDContabil.SetOnError(const Value: TErrorEvent);
begin
  FOnError := Value;

  FBloco_0.OnError := Value;
  FBloco_I.OnError := Value;
  FBloco_J.OnError := Value;
  FBloco_K.OnError := Value;
  FBloco_9.OnError := Value;
end;

procedure TACBrSPEDContabil.SaveFileTXT;
begin
  try
    IniciaGeracao;

    WriteBloco_0;
    WriteBloco_I;
    WriteBloco_J;
    WriteBloco_K;
    WriteBloco_9;

    TotalizarTermos;

    finally
      LimpaRegistros;
      FACBrTXT.Conteudo.Clear;
      FInicializado := False ;
  end;
end;

procedure TACBrSPEDContabil.WriteRegistro0000;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '0000';
      QTD_REG_BLC := 1;
   end;
   Bloco_0.WriteRegistro0000;
end;

procedure TACBrSPEDContabil.WriteRegistro0001;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '0001';
      QTD_REG_BLC := 1;
   end;
   Bloco_0.WriteRegistro0001;
end;

procedure TACBrSPEDContabil.WriteRegistro0007;
begin
   if Bloco_0.Registro0007.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := '0007';
         QTD_REG_BLC := Bloco_0.Registro0007.Count;
      end;
   end;
   Bloco_0.WriteRegistro0007;
end;

procedure TACBrSPEDContabil.WriteRegistro0020;
begin
   if Bloco_0.Registro0020.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := '0020';
         QTD_REG_BLC := Bloco_0.Registro0020.Count;
      end;
   end;
   Bloco_0.WriteRegistro0020;
end;

procedure TACBrSPEDContabil.WriteRegistro0035;
begin
   if Bloco_0.Registro0035.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := '0035';
         QTD_REG_BLC := Bloco_0.Registro0035.Count;
      end;
   end;
   Bloco_0.WriteRegistro0035;
end;

procedure TACBrSPEDContabil.WriteRegistro0150;
begin

   Bloco_0.WriteRegistro0150;

   if Bloco_0.Registro0150.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := '0150';
         QTD_REG_BLC := Bloco_0.Registro0150.Count;
      end;
   end;

   if Bloco_0.Registro0180Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := '0180';
         QTD_REG_BLC := Bloco_0.Registro0180Count;
      end;
   end;

end;

{
procedure TACBrSPEDContabil.WriteRegistro0180;
begin
   if Bloco_0.Registro0180.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := '0180';
         QTD_REG_BLC := Bloco_0.Registro0180.Count;
      end;
   end;
   Bloco_0.WriteRegistro0180;
end;
}

procedure TACBrSPEDContabil.WriteRegistro0990;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '0990';
      QTD_REG_BLC := 1;
   end;
   Bloco_0.WriteRegistro0990;
end;

procedure TACBrSPEDContabil.WriteRegistroI001;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'I001';
      QTD_REG_BLC := 1;
   end;
   Bloco_I.WriteRegistroI001;
end;

procedure TACBrSPEDContabil.WriteRegistroI010;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'I010';
      QTD_REG_BLC := 1;
   end;
   Bloco_I.WriteRegistroI010;
end;

procedure TACBrSPEDContabil.WriteRegistroI012;
begin
  Bloco_I.WriteRegistroI012;
  
  if Bloco_I.RegistroI012.Count > 0 then
  begin
    with Bloco_9.Registro9900.New do
    begin
       REG_BLC := 'I012';
       QTD_REG_BLC := Bloco_I.RegistroI012.Count;
    end;
  end;
  if Bloco_I.RegistroI015Count > 0 then
  begin
    with Bloco_9.Registro9900.New do
    begin
       REG_BLC := 'I015';
       QTD_REG_BLC := Bloco_I.RegistroI015Count;
    end;
  end;
end;

procedure TACBrSPEDContabil.WriteRegistroI020;
begin
   if Bloco_I.RegistroI020.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I020';
         QTD_REG_BLC := Bloco_I.RegistroI020.Count;
      end;
   end;
   Bloco_I.WriteRegistroI020;
end;

procedure TACBrSPEDContabil.WriteRegistroI030;
begin
   // Total de linhas do arquivo
   Bloco_I.RegistroI030.QTD_LIN := Bloco_0.Registro0990.QTD_LIN_0 +
                                   Bloco_I.RegistroI990.QTD_LIN_I +
                                   Bloco_J.RegistroJ990.QTD_LIN_J +
                                   Bloco_K.RegistroK990.QTD_LIN_K +
                                   Bloco_9.Registro9990.QTD_LIN_9;
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'I030';
      QTD_REG_BLC := 1;
   end;
   Bloco_I.WriteRegistroI030;
end;

procedure TACBrSPEDContabil.WriteRegistroI050;
begin
   Bloco_I.WriteRegistroI050;
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
   if Bloco_I.RegistroI052Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I052';
         QTD_REG_BLC := Bloco_I.RegistroI052Count;
      end;
   end;
end;

procedure TACBrSPEDContabil.WriteRegistroI075;
begin
   if Bloco_I.RegistroI075.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I075';
         QTD_REG_BLC := Bloco_I.RegistroI075.Count;
      end;
   end;
   Bloco_I.WriteRegistroI075;
end;

procedure TACBrSPEDContabil.WriteRegistroI100;
begin
   if Bloco_I.RegistroI100.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I100';
         QTD_REG_BLC := Bloco_I.RegistroI100.Count;
      end;
   end;
   Bloco_I.WriteRegistroI100;
end;

procedure TACBrSPEDContabil.WriteRegistroI150;
begin
   Bloco_I.WriteRegistroI150;
   if Bloco_I.RegistroI150.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I150';
         QTD_REG_BLC := Bloco_I.RegistroI150.Count;
      end;
   end;
   if Bloco_I.RegistroI151Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I151';
         QTD_REG_BLC := Bloco_I.RegistroI151Count;
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
   if Bloco_I.RegistroI157Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I157';
         QTD_REG_BLC := Bloco_I.RegistroI157Count;
      end;
   end;
end;

procedure TACBrSPEDContabil.WriteRegistroI200;
begin
   Bloco_I.WriteRegistroI200;
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
end;

procedure TACBrSPEDContabil.WriteRegistroI300;
begin
   Bloco_I.WriteRegistroI300;
   if Bloco_I.RegistroI300.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I300';
         QTD_REG_BLC := Bloco_I.RegistroI300.Count;
      end;
   end;
   if Bloco_I.RegistroI310Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I310';
         QTD_REG_BLC := Bloco_I.RegistroI310Count;
      end;
   end;
end;

procedure TACBrSPEDContabil.WriteRegistroI350;
begin
   Bloco_I.WriteRegistroI350;
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
end;

procedure TACBrSPEDContabil.WriteRegistroI500;
begin
   if Bloco_I.RegistroI500.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I500';
         QTD_REG_BLC := Bloco_I.RegistroI500.Count;
      end;
   end;
   Bloco_I.WriteRegistroI500;
end;


procedure TACBrSPEDContabil.WriteRegistroI510;
begin
   if Bloco_I.RegistroI510.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I510';
         QTD_REG_BLC := Bloco_I.RegistroI510.Count;
      end;
   end;
   Bloco_I.WriteRegistroI510;
end;

procedure TACBrSPEDContabil.WriteRegistroI550;
begin
   if Bloco_I.RegistroI550.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I550';
         QTD_REG_BLC := Bloco_I.RegistroI550.Count;
      end;
   end;

   if Bloco_I.RegistroI555Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'I555';
         QTD_REG_BLC := Bloco_I.RegistroI555Count;
      end;
   end;

   Bloco_I.WriteRegistroI550;
end;

procedure TACBrSPEDContabil.WriteRegistroI990;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'I990';
      QTD_REG_BLC := 1;
   end;
   Bloco_I.WriteRegistroI990;
end;

procedure TACBrSPEDContabil.WriteRegistroJ001;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'J001';
      QTD_REG_BLC := 1;
   end;
   Bloco_J.WriteRegistroJ001;
end;

procedure TACBrSPEDContabil.WriteRegistroJ005;
begin
   Bloco_J.WriteRegistroJ005;
   if Bloco_J.RegistroJ005.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'J005';
         QTD_REG_BLC := Bloco_J.RegistroJ005.Count;
      end;
   end;
   if Bloco_J.RegistroJ100Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'J100';
         QTD_REG_BLC := Bloco_J.RegistroJ100Count;
      end;
   end;
   if Bloco_J.RegistroJ150Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'J150';
         QTD_REG_BLC := Bloco_J.RegistroJ150Count;
      end;
   end;
   if Bloco_J.RegistroJ200Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'J200';
         QTD_REG_BLC := Bloco_J.RegistroJ200Count;
      end;
   end;
   if Bloco_J.RegistroJ210Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'J210';
         QTD_REG_BLC := Bloco_J.RegistroJ210Count;
      end;
   end;
   if Bloco_J.RegistroJ215Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'J215';
         QTD_REG_BLC := Bloco_J.RegistroJ215Count;
      end;
   end;
   if Bloco_J.RegistroJ800Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'J800';
         QTD_REG_BLC := Bloco_J.RegistroJ800Count;
      end;
   end;

   if Bloco_J.RegistroJ801Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'J801';
         QTD_REG_BLC := Bloco_J.RegistroJ801Count;
      end;
   end;


end;

{
procedure TACBrSPEDContabil.WriteRegistroJ800;
begin
   if Bloco_J.RegistroJ800.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'J800';
         QTD_REG_BLC := Bloco_J.RegistroJ800.Count;
      end;
   end;
   Bloco_J.WriteRegistroJ800;
end;

procedure TACBrSPEDContabil.WriteRegistroJ801;
begin
   if Bloco_J.RegistroJ801.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'J801';
         QTD_REG_BLC := Bloco_J.RegistroJ801.Count;
      end;
   end;
   Bloco_J.WriteRegistroJ801;
end;
}

procedure TACBrSPEDContabil.WriteRegistroJ900;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'J900';
      QTD_REG_BLC := 1;
   end;
   Bloco_J.WriteRegistroJ900;
end;

procedure TACBrSPEDContabil.WriteRegistroJ930;
begin
   if Bloco_J.RegistroJ930.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'J930';
         QTD_REG_BLC := Bloco_J.RegistroJ930.Count;
      end;
   end;
   Bloco_J.WriteRegistroJ930;
end;

procedure TACBrSPEDContabil.WriteRegistroJ932;
begin
   if Bloco_J.RegistroJ932.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'J932';
         QTD_REG_BLC := Bloco_J.RegistroJ932.Count;
      end;
   end;
   Bloco_J.WriteRegistroJ932;
end;

procedure TACBrSPEDContabil.WriteRegistroJ935;
begin
   if Bloco_J.RegistroJ935.Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'J935';
         QTD_REG_BLC := Bloco_J.RegistroJ935.Count;
      end;
   end;
   Bloco_J.WriteRegistroJ935;
end;

procedure TACBrSPEDContabil.WriteRegistroJ990;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'J990';
      QTD_REG_BLC := 1;
   end;
   Bloco_J.WriteRegistroJ990;
end;

procedure TACBrSPEDContabil.WriteRegistroK001;
begin
   if ((DT_INI >= EncodeDate(2016, 1, 1)) and (Bloco_0.Registro0000.IND_ESC_CONS = 'S')) then
      if ((Bloco_K.RegistroK001.IND_DAD = 0) or (DT_INI >= EncodeDate(2017, 1, 1))) then
      begin
         with Bloco_9.Registro9900.New do
         begin
            REG_BLC := 'K001';
            QTD_REG_BLC := 1;
         end;
         Bloco_K.WriteRegistroK001;
      end;
end;

procedure TACBrSPEDContabil.WriteRegistroK030;
begin
   if Bloco_K.RegistroK001.IND_DAD = 0 then
   begin
      Bloco_K.WriteRegistroK030;

      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'K030';
         QTD_REG_BLC := 1;
      end;
   end;
   if Bloco_K.RegistroK100Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'K100';
         QTD_REG_BLC := Bloco_K.RegistroK100Count;
      end;
   end;
   if Bloco_K.RegistroK110Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'K110';
         QTD_REG_BLC := Bloco_K.RegistroK110Count;
      end;
   end;
   if Bloco_K.RegistroK115Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'K115';
         QTD_REG_BLC := Bloco_K.RegistroK115Count;
      end;
   end;
   if Bloco_K.RegistroK200Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'K200';
         QTD_REG_BLC := Bloco_K.RegistroK200Count;
      end;
   end;
   if Bloco_K.RegistroK210Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'K210';
         QTD_REG_BLC := Bloco_K.RegistroK210Count;
      end;
   end;
   if Bloco_K.RegistroK300Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'K300';
         QTD_REG_BLC := Bloco_K.RegistroK300Count;
      end;
   end;
   if Bloco_K.RegistroK310Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'K310';
         QTD_REG_BLC := Bloco_K.RegistroK310Count;
      end;
   end;
   if Bloco_K.RegistroK315Count > 0 then
   begin
      with Bloco_9.Registro9900.New do
      begin
         REG_BLC := 'K315';
         QTD_REG_BLC := Bloco_K.RegistroK315Count;
      end;
   end;
end;

procedure TACBrSPEDContabil.WriteRegistroK990;
begin
   if ((DT_INI >= EncodeDate(2016, 1, 1)) and (Bloco_0.Registro0000.IND_ESC_CONS = 'S')) then
      if ((Bloco_K.RegistroK001.IND_DAD = 0) or (DT_INI >= EncodeDate(2017, 1, 1))) then
      begin
         with Bloco_9.Registro9900.New do
         begin
            REG_BLC := 'K990';
            QTD_REG_BLC := 1;
         end;
         Bloco_K.WriteRegistroK990;
      end;  
end;

procedure TACBrSPEDContabil.WriteRegistro9001;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '9001';
      QTD_REG_BLC := 1;
   end;
   Bloco_9.WriteRegistro9001;
end;

procedure TACBrSPEDContabil.WriteRegistro9900;
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
   Bloco_9.WriteRegistro9900;
end;

procedure TACBrSPEDContabil.WriteRegistro9990;
begin
   Bloco_9.WriteRegistro9990;
end;

procedure TACBrSPEDContabil.WriteRegistro9999;
begin
   Bloco_9.Registro9999.QTD_LIN := Bloco_9.Registro9999.QTD_LIN + Bloco_0.Registro0990.QTD_LIN_0 +
                                                                  Bloco_I.RegistroI990.QTD_LIN_I +
                                                                  Bloco_J.RegistroJ990.QTD_LIN_J +
                                                                  Bloco_K.RegistroK990.QTD_LIN_K +
                                                                  Bloco_9.Registro9990.QTD_LIN_9;
   Bloco_9.WriteRegistro9999;
end;

function TACBrSPEDContabil.GetLinhasBuffer: Integer;
begin
   Result := FACBrTXT.LinhasBuffer ;
end;

procedure TACBrSPEDContabil.SetLinhasBuffer(const Value: Integer);
begin
   FACBrTXT.LinhasBuffer := Value ;
end;

procedure TACBrSPEDContabil.InicializaBloco(Bloco: TACBrSPED);
begin
   Bloco.NomeArquivo  := FACBrTXT.NomeArquivo;
   Bloco.LinhasBuffer := FACBrTXT.LinhasBuffer;
   Bloco.Gravado      := False ;
   if not Assigned(Bloco.Conteudo) then
     Bloco.Conteudo := TStringList.Create;
   Bloco.Conteudo.Clear;
end;

procedure TACBrSPEDContabil.IniciaGeracao;
var
  intFor: integer;
begin
  if FInicializado then exit;
  
  if (Trim(Arquivo) = '') or (Trim(fPath) = '') then
     raise Exception.Create( ACBrStr('Caminho ou nome do arquivo não informado!'));

  FACBrTXT.NomeArquivo := FPath + Arquivo ;
  FACBrTXT.Reset;    // Apaga o Arquivo e limpa memória

  InicializaBloco( Bloco_0 ) ;
  InicializaBloco( Bloco_I ) ;
  InicializaBloco( Bloco_J ) ;
  InicializaBloco( Bloco_K ) ;
  InicializaBloco( Bloco_9 ) ;

  /// Preparação para totalizações de registros.
  Bloco_0.Registro0990.QTD_LIN_0 := 0;
  Bloco_I.RegistroI990.QTD_LIN_I := 0;
  Bloco_J.RegistroJ990.QTD_LIN_J := 0;
  Bloco_K.RegistroK990.QTD_LIN_K := 0;
  Bloco_9.Registro9990.QTD_LIN_9 := 0;
  Bloco_9.Registro9999.QTD_LIN   := 0;

  for intFor := 0 to Bloco_9.Registro9900.Count - 1 do
  begin
     Bloco_9.Registro9900.Items[intFor] := nil;
     Bloco_9.Registro9900.Items[intFor].Free;
  end;
  
  Bloco_9.Registro9900.Clear;

  FInicializado := True;
end;

procedure TACBrSPEDContabil.WriteBloco_0;
begin
  if Bloco_0.Gravado then exit ;

  if not FInicializado then
     raise Exception.Create( 'Métodos "IniciaGeracao" não foi executado' );

  WriteRegistro0000;
  WriteRegistro0001;
  WriteRegistro0007;
  WriteRegistro0035;
  WriteRegistro0020;
  WriteRegistro0150;
  //WriteRegistro0180;
  WriteRegistro0990;

  Bloco_0.WriteBuffer;
  Bloco_0.Conteudo.Clear;
  Bloco_0.Gravado := true;
end;

procedure TACBrSPEDContabil.SetArquivo(const Value: String);
begin
  FArquivo := Value;
end;

procedure TACBrSPEDContabil.WriteBloco_I;
begin
  if Bloco_I.Gravado then Exit;

  if not Bloco_I.Gravado then
    WriteBloco_0;

  WriteRegistroI001;
  WriteRegistroI010;
  WriteRegistroI012;
  WriteRegistroI020;
  WriteRegistroI030;
  WriteRegistroI050;
  WriteRegistroI075;
  WriteRegistroI100;
  WriteRegistroI150;
  WriteRegistroI200;
  WriteRegistroI300;
  WriteRegistroI350;
  WriteRegistroI500;
  WriteRegistroI510;
  WriteRegistroI550;
  WriteRegistroI990;

  Bloco_I.WriteBuffer;
  Bloco_I.Conteudo.Clear;
  Bloco_I.Gravado := true;
end;

procedure TACBrSPEDContabil.WriteBloco_J;
begin
  if Bloco_J.Gravado then Exit;

  if not Bloco_J.Gravado then
    WriteBloco_I;

  WriteRegistroJ001;
  WriteRegistroJ005;
//  WriteRegistroJ800;
//  WriteRegistroJ801;
  WriteRegistroJ900;
  WriteRegistroJ930;
  WriteRegistroJ935;
  WriteRegistroJ990;

  Bloco_J.WriteBuffer;
  Bloco_J.Conteudo.Clear;
  Bloco_J.Gravado := true;
end;

procedure TACBrSPEDContabil.WriteBloco_K;
begin
  if Bloco_K.Gravado then Exit;

  if not Bloco_K.Gravado then
    WriteBloco_J;

  WriteRegistroK001;
  WriteRegistroK030;
  WriteRegistroK990;

  Bloco_K.WriteBuffer;
  Bloco_K.Conteudo.Clear;
  Bloco_K.Gravado := true;
end;

procedure TACBrSPEDContabil.WriteBloco_9;
begin
  if Bloco_9.Gravado then Exit;

  if not Bloco_9.Gravado then
    WriteBloco_K;

  WriteRegistro9001;
  WriteRegistro9900;
  WriteRegistro9990;
  WriteRegistro9999;

  Bloco_9.WriteBuffer;
  Bloco_9.Conteudo.Clear;
  Bloco_9.Gravado := true;
end;

function TACBrSPEDContabil.GetConteudo: TStringList;
begin
  Result := FACBrTXT.Conteudo;
end;

procedure TACBrSPEDContabil.TotalizarTermos;
var
  fs: TFileStream;
  sByte, sByteNew: Byte;
  iInc, iEnd, iCont, iIni : Integer;
  sTotal, sChar : String;
begin
  sTotal := FACBrTXT.LFill(Bloco_9.Registro9999.QTD_LIN, 9, false);
  sChar := '';
  iCont:=0;
  fs := TFileStream.Create(FACBrTXT.NomeArquivo, fmOpenReadWrite or fmShareExclusive );

//******************************************************************************
// iEnd : soma a quantidade de vezes que encontra o caracter '[' dentro do
// arquivo, no momento que enontra 2 vezes encerra a alteração dos totalizadores
// do arquivo e encerra o laço de repetição.
//******************************************************************************
  try
    iEnd := 0;
    while iEnd <> 2 do
    begin
      fs.Position := iCont;
      fs.Read(sByte, 1);
      if (Chr(sByte) = '[') then
      begin
        fs.Position := iCont;
        iIni := iCont;
        sChar := '';

        for iInc := 1 to 9 do
        begin
          fs.Position := iCont;
          fs.Read(sByte, 1);
          sChar := sChar + Chr(sByte);
          Inc(iCont);
        end;

        if (sChar = '[*******]') then
        begin
          for iInc := 1 to 9 do
          begin
            fs.Position := iIni;
            sByteNew := Ord(sTotal[iInc]);
            fs.Write(sByteNew,1);
            Inc(iIni);
          end;
          Inc(iEnd);
        end;
      end;
      Inc(iCont);
    end;
    finally
    begin
      FreeAndNil(fs);
    end;
  end;
end;

{$ifdef FPC}
initialization
   {$I ACBrSpedContabil.lrs}
{$endif}

end.

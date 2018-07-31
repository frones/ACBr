unit DoACBrDFeUnit;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, CmdUnit, ACBrUtil, ACBrMonitorConfig,
  ACBrMonitorConsts, ACBrDFeUtil, UtilUnit, ACBrDFe;

type

{ TACBrCarregarDFe }

TACBrCarregarDFe = class
protected
  FXMLorFile           : String;
  FPathDFe             : String;
  FPathDFeExtensao     : String;
  FRetornaFalha        : Boolean;

  FpACBrDFe: TACBrDFe;
  FMsgValidaPath : TStringList;

  procedure CarregarDFePath( const AValue: String ); virtual;
  procedure CarregarDFeXML( const AValue: String ); virtual;
  function ValidarDFe( const AValue: String ): Boolean; virtual;

  function ValidarPath(const AValue: String): Boolean;
  procedure SetFXMLorFile(const AValue: String);

  property XMLorFile : String read FXMLorFile write SetFXMLorFile;

public
  constructor Create(AACBrDFe: TACBrDFe; AXMLorFile: String; ARetornaFalha: Boolean = True );
  Destructor  Destroy; Override;

  property PathDFe        : String read FPathDFe;
  property PathDFeExtensao: String read FPathDFeExtensao;
  property RetornaFalha   : Boolean read FRetornaFalha;

end;

implementation

uses
  DoACBrUnit;

{ TACBrCarregarDFe }

procedure TACBrCarregarDFe.CarregarDFePath(const AValue: String);
begin
  raise Exception.Create(SErroNaoImplementado);
end;

procedure TACBrCarregarDFe.CarregarDFeXML(const AValue: String);
begin
  raise Exception.Create(SErroNaoImplementado);
end;

function TACBrCarregarDFe.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  raise Exception.Create(SErroNaoImplementado);
end;

function TACBrCarregarDFe.ValidarPath(const AValue: String): Boolean;
begin
  Result := False;
  if (FilesExists( AValue ) ) then
    Result:= True
  else
    FMsgValidaPath.Add( Format( SErroArqNaoEncontado, [AValue] ) );
end;

procedure TACBrCarregarDFe.SetFXMLorFile(const AValue: String);
var
  IsXML : Boolean;
begin
  try
    IsXML := StringIsXML(AValue);

    if not(IsXML) then
    begin
      try
        if ValidarPath(AValue) then
          CarregarDFePath(AValue);

        if not(ValidarDFe(AValue) ) then
        begin
          if ValidarPath(FPathDFe) then
            CarregarDFePath(FPathDFe);
        end;

        if not(ValidarDFe(AValue) ) then
        begin
          if ValidarPath(FPathDFeExtensao) then
            CarregarDFePath(FPathDFeExtensao)
          else if RetornaFalha then
            raise Exception.Create( ACBrStr( FMsgValidaPath.Text ) );
        end;

      finally
        FMsgValidaPath.Clear;
      end;
    end
    else
      CarregarDFeXML(AValue);

  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;

constructor TACBrCarregarDFe.Create(AACBrDFe: TACBrDFe; AXMLorFile: String; ARetornaFalha: Boolean);
begin
  inherited Create;
  FMsgValidaPath   := TStringList.Create;
  FPathDFe         := '';
  FPathDFeExtensao := '';
  FRetornaFalha    := ARetornaFalha;
  FpACBrDFe := AACBrDFe;
  XMLorFile := AXMLorFile;
end;

destructor TACBrCarregarDFe.Destroy;
begin
  inherited;
  FMsgValidaPath.Free;
end;


end.


unit SCVBioControlID;

{$MODE Delphi}

interface

uses
  Forms, Windows, ControliD, SysUtils;

type
  EiDBioException = class(Exception);

  //validar digital
  function ValidarDigital(cDigital1: String; cDigital2: String): Boolean;

  //retornar digital
  function RetornarDigital: String;

  //retornar o estado do leitor
  function VerificaLeitor: RetCode;

  //retora os dados do leitor
  function DadosLeitor: string;

implementation

function ValidarDigital(cDigital1: String; cDigital2: String): Boolean;
var
  Ret: RetCode;
  aScore: Int32;
  bio : CIDBio;
begin
  Result := False;

  Ret := CIDBIO.Init;
  try
    if Ret >= RetCode.SUCCESS then
    begin
      bio := CIDBio.Create;
      Ret := bio.MatchTemplates(cDigital1, cDigital2, aScore);
      Result := (Ret = RetCode.SUCCESS);
    end
    else
      Application.MessageBox(pchar('Erro no leitor: '+ CIDBio.GetErrorMessage(Ret)),'Atenção', MB_ICONWARNING);
  finally
    CIDBIO.Terminate;
  end;

end;

function RetornarDigital: String;
var
  Ret: RetCode;
  stemplate: String;
  aBytes: ByteArray;
  aWidth, aHeigt: Int32;
  aQualidade: Int32;
  bio : CIDBio;
begin
  Result := '';

  Ret := CIDBIO.Init;
  try
    if Ret >= RetCode.SUCCESS then
    begin
      bio := CIDBio.Create;
      Ret := bio.CaptureImageAndTemplate(stemplate, aBytes, aWidth, aHeigt, aQualidade);
      if Ret = RetCode.SUCCESS then
      begin
        Result := stemplate;
      end
      else
        Application.MessageBox(pchar('Erro no leitor: '+ CIDBio.GetErrorMessage(Ret)),'Atenção', MB_ICONWARNING);
    end
    else
      raise EiDBioException.Create(pchar('Erro no leitor: '+ CIDBio.GetErrorMessage(Ret)));
      //Application.MessageBox(pchar('Erro no leitor: '+ CIDBio.GetErrorMessage(Ret)),'Atenção', MB_ICONWARNING);
  finally
    CIDBIO.Terminate;
  end;

end;

function VerificaLeitor : RetCode;
begin
  try
    Result := CIDBIO.Init;
  finally
    CIDBIO.Terminate;
  end;

end;

function DadosLeitor: string;
var
  version: String;
  serialNumber: String;
  model: String;
  Ret: RetCode;
  bio : CIDBIO;
begin
  Result := '';

  Ret := CIDBIO.Init;
  try
    if Ret >= RetCode.SUCCESS then
    begin
      bio := CIDBio.Create;
      Ret := bio.GetDeviceInfo(version, serialNumber, model);
      if Ret = RetCode.SUCCESS then
      begin
        Result := 'Versão Frimware: '+ version +sLineBreak+
                  'Nº série: '+ serialNumber +sLineBreak+
                  'Modelo: '+ model;
      end
      else
        Application.MessageBox(pchar('Erro no leitor: ' + CIDBIO.GetErrorMessage(Ret)),'Atenção', MB_ICONWARNING);
    end
    else
      Application.MessageBox(pchar('Erro no leitor: ' + CIDBIO.GetErrorMessage(Ret)),'Atenção', MB_ICONWARNING);
  finally
    CIDBIO.Terminate;
  end;

end;

end.

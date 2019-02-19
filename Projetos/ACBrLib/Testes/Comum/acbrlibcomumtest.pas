unit ACBrLibComumTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Graphics, ACBrLibResposta;

type

  { TACbrLibRespostaDescendenteSimples }

  TACbrLibRespostaDescendenteSimples = class(TACBrLibResposta)
  private
    FFonte: TFont;
    FFs: TFontStyle;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);
    destructor Destroy; override;
  published
    property Fs: TFontStyle read FFs write FFs;
    property Fonte: TFont read FFonte write FFonte;

  end;

  TACBrLibResposta_Testes= class(TTestCase)
  published
    procedure GravarIni_TesteFonte;
  end;

implementation

uses IniFiles;

{ TACbrLibRespostaDescendenteSimples }

constructor TACbrLibRespostaDescendenteSimples.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited;
  FFonte := TFont.Create;
end;

destructor TACbrLibRespostaDescendenteSimples.Destroy;
begin
  FFonte.Free;
  inherited Destroy;
end;

procedure TACBrLibResposta_Testes.GravarIni_TesteFonte;
var
  acrds: TACbrLibRespostaDescendenteSimples;
  Resultado, STeste: string;
  ITeste: Int64;
  AIni: TMemIniFile;
  Astr: TStringStream;
begin
  acrds := TACbrLibRespostaDescendenteSimples.Create('Sessao', resINI);
  try
    acrds.Fonte.Name := 'Arial';
    acrds.Fonte.Style := [fsStrikeOut, fsItalic];
    acrds.Fs := fsUnderline;
    Resultado := acrds.Gerar;
  finally
    acrds.Free;
  end;
  Astr := TStringStream.Create(Resultado);
  try
    AIni := TMemIniFile.Create(Astr);
    try
      STeste := AIni.ReadString('Fonte', 'Name', '');
      CheckEquals('Arial', STeste, 'Falhou Fonte.Name!');

      STeste := AIni.ReadString('Fonte', 'Style', '');
      CheckEquals('[1,3]', STeste, 'Falhou Fonte.Style!');

      ITeste := AIni.ReadInt64('Sessao', 'Fs', -1);
      CheckEquals(2, ITeste, 'Falhou Fs (enumerado).');

    finally
      AIni.Free;
    end;
  finally
    Astr.Free;
  end;

end;



initialization

  RegisterTest(TACBrLibResposta_Testes);
end.


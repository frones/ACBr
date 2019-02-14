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
    FFs: TFontStyles;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);
    destructor Destroy; override;
  published
    property Fs: TFontStyles read FFs write FFs;
    property Fonte: TFont read FFonte write FFonte;

  end;

  TACBrLibResposta_Testes= class(TTestCase)
  published
    procedure GravarIni_TesteFonte;
  end;

implementation

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
  Resultado: string;
begin
  acrds := TACbrLibRespostaDescendenteSimples.Create('Sessao', resINI);
  try
    acrds.Fonte.Name := 'Arial';
    acrds.Fonte.Style := [fsBold];
    acrds.Fs := [fsItalic, fsUnderline];
    Resultado := acrds.Gerar;
  finally
    acrds.Free;
  end;
  CheckEquals('ValorEsperado', Resultado, 'Erro?');

end;



initialization

  RegisterTest(TACBrLibResposta_Testes);
end.


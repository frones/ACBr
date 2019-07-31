
{$I ACBr.inc}

unit pcnCIOTR;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  ACBrUtil, pcnAuxiliar, pcnConversao, pcnLeitor, pcnConversaoCIOT, pcnCIOT;

type

  TCIOTR = class(TPersistent)
  private
    FLeitor: TLeitor;
    FCIOT: TCIOT;
  public
    constructor Create(AOwner: TCIOT);
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property CIOT: TCIOT       read FCIOT    write FCIOT;
  end;

implementation

{ TCIOTR }

constructor TCIOTR.Create(AOwner: TCIOT);
begin
  inherited Create;
  FLeitor := TLeitor.Create;
  FCIOT := AOwner;
end;

destructor TCIOTR.Destroy;
begin
  FLeitor.Free;

  inherited Destroy;
end;

function TCIOTR.LerXml: Boolean;
begin
  Leitor.Grupo := Leitor.Arquivo;

//  CIOT.usuario := Leitor.rCampo(tcStr, 'usuario');
//  CIOT.senha   := Leitor.rCampo(tcStr, 'senha');
//  CIOT.codatm  := Leitor.rCampo(tcStr, 'codatm');
//
//  CIOT.xmlDFe := Leitor.rCampo(tcStr, 'xmlCTe');
//
//  if CIOT.xmlDFe = '' then
//    CIOT.xmlDFe := Leitor.rCampo(tcStr, 'xmlNFe');
//
//  if CIOT.xmlDFe = '' then
//    CIOT.xmlDFe := Leitor.rCampo(tcStr, 'xmlMDFe');
//
//  CIOT.xmlDFe := StringReplace(CIOT.xmlDFe, '<![CDATA[', '', [rfReplaceAll]);
//  CIOT.xmlDFe := StringReplace(CIOT.xmlDFe, ']]>', '', [rfReplaceAll]);
//
//  CIOT.aplicacao     := Leitor.rCampo(tcStr, 'aplicacao');
//  CIOT.assunto       := Leitor.rCampo(tcStr, 'assunto');
//  CIOT.remetentes    := Leitor.rCampo(tcStr, 'remetentes');
//  CIOT.destinatarios := Leitor.rCampo(tcStr, 'destinatarios');
//  CIOT.corpo         := Leitor.rCampo(tcStr, 'corpo');
//  CIOT.chave         := Leitor.rCampo(tcStr, 'chave');
//  CIOT.chaveresp     := Leitor.rCampo(tcStr, 'chaveresp');

  Result := True;
end;

end.


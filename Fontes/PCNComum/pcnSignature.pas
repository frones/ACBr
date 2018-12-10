////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org/nfe                                   //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcnSignature;

interface

uses
  SysUtils, Classes,  pcnConversao, pcnGerador, pcnConsts;

type

  { TSignature }

  TSignature = class(TPersistent)
  private
    FGerador: TGerador;
    FURI: String;
    FDigestValue: String;
    FSignatureValue: String;
    FX509Certificate: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GerarXML: Boolean;
    procedure Clear;
  published
    property Gerador: TGerador       read FGerador         write FGerador;
    property URI: String             read FURI             write FURI;
    property DigestValue: String     read FDigestValue     write FDigestValue;
    property SignatureValue: String  read FSignatureValue  write FSignatureValue;
    property X509Certificate: String read FX509Certificate write FX509Certificate;
  end;

implementation

{ TSignature }

constructor TSignature.Create;
begin
  inherited;
  FGerador := TGerador.Create;
end;

destructor TSignature.Destroy;
begin
  FGerador.Free;
  inherited;
end;

procedure TSignature.Clear;
begin
  FURI             := '';
  FDigestValue     := '';
  FSignatureValue  := '';
  FX509Certificate := '';
end;

function TSignature.GerarXML: Boolean;
begin
  FGerador.ArquivoFormatoXML := '';
  FGerador.Opcoes.TagVaziaNoFormatoResumido := false;
  FGerador.FIgnorarTagIdentacao := '|Reference URI|SignatureMethod|Transform Algorithm="http://www.w3.org/TR|/Transforms|/Reference|';

  (**)Gerador.wGrupo('Signature xmlns="http://www.w3.org/2000/09/xmldsig#"', 'XS01');
  (****)Gerador.wGrupo('SignedInfo', 'XS02');
  (******)Gerador.wGrupo('CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/', 'XS03');
  (******)Gerador.wGrupo('SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/', 'XS05');
  (******)Gerador.wGrupo('Reference URI="#NFe' + FURI + '"', 'XS07');
  (********)Gerador.wGrupo('Transforms', 'XS10');
  (**********)Gerador.wGrupo('Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/', 'SX12');
  (**********)Gerador.wGrupo('Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/', 'SX12');
  (********)Gerador.wGrupo('/Transforms');
  (********)Gerador.wGrupo('DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/', 'XS15');
  (********)Gerador.wCampo(tcStr, 'XS17', 'DigestValue', 000, 999, 1, FDigestValue, DSC_DigestValue, False);
  (******)Gerador.wGrupo('/Reference');
  (****)Gerador.wGrupo('/SignedInfo');
  (****)Gerador.wCampo(tcStr, 'XS18', 'SignatureValue', 000, 999, 1, FSignatureValue, DSC_SignatureValue, False);
  (****)Gerador.wGrupo('KeyInfo', 'XS19');
  (******)Gerador.wGrupo('X509Data', 'XS20');
  (********)Gerador.wCampo(tcStr, 'XS21', 'X509Certificate', 000, 999, 1, FX509Certificate, DSC_X509Certificate, False);
  (******)Gerador.wGrupo('/X509Data');
  (****)Gerador.wGrupo('/KeyInfo');
  (**)Gerador.wGrupo('/Signature');
  
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

procedure TSignature.Assign(Source: TPersistent);
begin
  if Source is TSignature then
  begin
//    Gerador.Assign(TSignature(Source).Gerador);
    URI := TSignature(Source).URI;
    DigestValue := TSignature(Source).DigestValue;
    SignatureValue := TSignature(Source).SignatureValue;
    X509Certificate := TSignature(Source).X509Certificate;
  end
  else
    inherited; 
end;

end.


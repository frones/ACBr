{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrDFeComum.RetEnvio;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase, ACBrXmlBase;

type

  TInfRec = class(TObject)
  private
    FnRec: string;
    FdhRecbto: TDateTime;
    FtMed: Integer;
  public
    property nRec: string        read FnRec     write FnRec;
    property dhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property tMed: Integer       read FtMed     write FtMed;
  end;

  TretEnvDFe = class(TObject)
  private
    Fversao: string;
    FtpAmb: TACBrTipoAmbiente;
    FcStat: Integer;
    FcUF: Integer;
    FverAplic: string;
    FxMotivo: string;
    FinfRec: TInfRec;
    FXmlRetorno: string;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: Boolean;

    property versao: string           read Fversao    write Fversao;
    property tpAmb: TACBrTipoAmbiente read FtpAmb    write FtpAmb;
    property verAplic: string         read FverAplic write FverAplic;
    property cStat: Integer           read FcStat    write FcStat;
    property xMotivo: string          read FxMotivo  write FxMotivo;
    property cUF: Integer             read FcUF      write FcUF;
    property infRec: TInfRec          read FinfRec   write FinfRec;

    property XmlRetorno: string read FXmlRetorno write FXmlRetorno;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrXmlDocument;

{ TretEnvDFe }

constructor TretEnvDFe.Create;
begin
  inherited Create;

  FinfRec := TInfREC.Create
end;

destructor TretEnvDFe.Destroy;
begin
  FinfRec.Free;

  inherited;
end;

function TretEnvDFe.LerXml: Boolean;
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  ok: Boolean;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if XmlRetorno = '' then Exit;

      Document.LoadFromXml(XmlRetorno);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        versao := ObterConteudoTag(ANode.Attributes.Items['versao']);
        tpAmb := StrToTipoAmbiente(ok, ObterConteudoTag(Anode.Childrens.FindAnyNs('tpAmb'), tcStr));
        cUF := ObterConteudoTag(Anode.Childrens.FindAnyNs('cUF'), tcInt);
        verAplic := ObterConteudoTag(ANode.Childrens.FindAnyNs('verAplic'), tcStr);
        cStat := ObterConteudoTag(ANode.Childrens.FindAnyNs('cStat'), tcInt);
        xMotivo := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMotivo'), tcStr);

        AuxNode := ANode.Childrens.FindAnyNs('infRec');

        if AuxNode <> nil then
        begin
          infRec.nRec := ObterConteudoTag(Anode.Childrens.FindAnyNs('nRec'), tcStr);
          infRec.dhRecbto := ObterConteudoTag(Anode.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
          infRec.tMed := ObterConteudoTag(ANode.Childrens.FindAnyNs('tMed'), tcInt);
        end;
      end;

      Result := True;
    except
      Result := False;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

end.


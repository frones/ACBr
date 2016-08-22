////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org                                       //
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

{*******************************************************************************
|* Historico
|*
|* 28/09/2012: Italo
|*  - Revisado geração do XML e adicionado propriedade para controle de Versão
|*    do WebService Utilizado
*******************************************************************************}

{$I ACBr.inc}

unit pcnCancNFe;

interface

uses
  SysUtils, Classes, pcnConversao, pcnGerador, ACBrDFeConsts;

type

  TcancNFe = class(TPersistent)
  private
    FGerador: TGerador;
    FChave: String;
    FtpAmb: TpcnTipoAmbiente;
    FchNFe: String;
    FnProt: String;
    FxJust: String;
    FVersao: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;
    function ObterNomeArquivo: String;
  published
    property Gerador: TGerador       read FGerador write FGerador;
    property Chave: String           read FChave   write FChave;
    property tpAmb: TpcnTipoAmbiente read FtpAmb   write FtpAmb;
    property chNFe: String           read FchNFe   write FchNFe;
    property nProt: String           read FnProt   write FnProt;
    property xJust: String           read FxJust   write FxJust;
    property Versao: String          read FVersao  write FVersao;
  end;

implementation

Uses pcnAuxiliar,
  ACBrUtil;

{ TcancNFe }

constructor TcancNFe.Create;
begin
  FGerador := TGerador.Create;
end;

destructor TcancNFe.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TcancNFe.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(FchNFe) + '-ped-can.xml';
end;

function TcancNFe.GerarXML: Boolean;
begin
  Gerador.ArquivoFormatoXML := '';

  Gerador.wGrupo('cancNFe ' + NAME_SPACE + ' versao="' + Versao + '"');
  Gerador.wGrupo('infCanc Id="ID' + OnlyNumber(FchNFe) + '"');
  Gerador.wCampo(tcStr, 'CP05', 'tpAmb', 001, 001, 1, tpAmbToStr(FtpAmb), DSC_TPAMB);
  Gerador.wCampo(tcStr, 'CP06', 'xServ', 008, 008, 1, 'CANCELAR', DSC_XSERV);
  Gerador.wCampo(tcEsp, 'CP07', 'chNFe', 044, 044, 1, OnlyNumber(FchNFe), DSC_CHNFE);
  if not ValidarChave(FchNFe) then
    Gerador.wAlerta('CP07', 'chNFe', '', 'Chave de NFe inválida');
  Gerador.wCampo(tcEsp, 'CP08', 'nProt', 015, 015, 1, OnlyNumber(FnProt), DSC_NPROT);
  Gerador.wCampo(tcStr, 'CP09', 'xJust', 015, 255, 1, FiltrarTextoXML(true, FxJust), DSC_XJUST);
  Gerador.wGrupo('/infCanc');
  Gerador.wGrupo('/cancNFe');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar CTe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da CTe          //
//                                                                            //
//        site: www.projetocooperar.org/cte                                   //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_cte/        //
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
//              "PCN  -  Projeto  Cooperar  CTe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcteCancCTe;

interface

uses
  SysUtils, Classes, pcnAuxiliar, pcnConversao, pcnGerador, ACBrUtil,
  pcteConversaoCTe, pcnConsts, pcteConsts;

type

  TcancCTe = class(TPersistent)
  private
    FGerador: TGerador;
    FChave: String;
    FtpAmb: TpcnTipoAmbiente;
    FchCTe: String;
    FnProt: String;
    FxJust: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;
  published
    property Gerador: TGerador       read FGerador write FGerador;
    property Chave: String           read FChave   write FChave;
    property tpAmb: TpcnTipoAmbiente read FtpAmb   write FtpAmb;
    property chCTe: String           read FchCTe   write FchCTe;
    property nProt: String           read FnProt   write FnProt;
    property xJust: String           read FxJust   write FxJust;
  end;

implementation

{ TcancCTe }

constructor TcancCTe.Create;
begin
  FGerador := TGerador.Create;
end;

destructor TcancCTe.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TcancCTe.GerarXML: Boolean;
begin
  Gerador.ArquivoFormatoXML := '';

	Gerador.wGrupo('cancCTe ' + NAME_SPACE_CTE + ' versao="1.04"');
  Gerador.wGrupo('infCanc Id="ID' + OnlyNumber(FchCTe) + '"');
  Gerador.wCampo(tcStr, 'CP05', 'tpAmb', 001, 001, 1, tpAmbToStr(FtpAmb), DSC_TPAMB);
  Gerador.wCampo(tcStr, 'CP06', 'xServ', 008, 008, 1, 'CANCELAR', DSC_XSERV);
  Gerador.wCampo(tcEsp, 'CP07', 'chCTe', 044, 044, 1, OnlyNumber(FchCTe), DSC_CHCTe);
  if not ValidarChave(FchCTe) then
      Gerador.wAlerta('CP07', 'chCTe', '', 'Chave do CTe inválida');
  Gerador.wCampo(tcEsp, 'CP08', 'nProt', 015, 015, 1, OnlyNumber(FnProt), DSC_NPROT);
  Gerador.wCampo(tcStr, 'CP09', 'xJust', 015, 255, 1, FiltrarTextoXML(true, FxJust), DSC_XJUST);
  Gerador.wGrupo('/infCanc');
  Gerador.wGrupo('/cancCTe');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.


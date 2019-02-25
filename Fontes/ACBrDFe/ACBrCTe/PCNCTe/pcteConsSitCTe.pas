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
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcteConsSitCTe;

interface

uses
  SysUtils, Classes, pcnAuxiliar, pcnConversao, pcnGerador, ACBrUtil,
  pcnConsts{, pcteConversaoCTe};

type

  TConsSitCTe = class
  private
    FGerador: TGerador;
    FtpAmb: TpcnTipoAmbiente;
    FchCTe: String;
    FVersao: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;

    property Gerador: TGerador       read FGerador write FGerador;
    property tpAmb: TpcnTipoAmbiente read FtpAmb   write FtpAmb;
    property chCTe: String           read FchCTe   write FchCTe;
    property Versao: String          read FVersao  write FVersao;
  end;

implementation

{ TConsSitCTe }

constructor TConsSitCTe.Create;
begin
  FGerador := TGerador.Create;
end;

destructor TConsSitCTe.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TConsSitCTe.GerarXML: Boolean;
begin
  Gerador.ArquivoFormatoXML := '';

  Gerador.wGrupo('consSitCTe ' + NAME_SPACE_CTE + ' versao="' + Versao + '"');
  Gerador.wCampo(tcStr, 'EP03', 'tpAmb', 001, 001, 1, tpAmbToStr(FtpAmb), DSC_TPAMB);
  Gerador.wCampo(tcStr, 'EP04', 'xServ', 009, 009, 1, 'CONSULTAR', DSC_XSERV);
  Gerador.wCampo(tcEsp, 'EP05', 'chCTe', 044, 044, 1, OnlyNumber(FchCTe), DSC_CHCTe);
  if not ValidarChave(FchCTe) then
    Gerador.wAlerta('EP05', 'chCTe', '', 'Chave do CTe inválida');
  Gerador.wGrupo('/consSitCTe');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.


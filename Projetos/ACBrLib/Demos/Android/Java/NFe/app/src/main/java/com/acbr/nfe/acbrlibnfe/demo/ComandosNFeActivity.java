package com.acbr.nfe.acbrlibnfe.demo;

import android.annotation.SuppressLint;
import android.os.Bundle;

import androidx.appcompat.app.AppCompatActivity;

import com.acbr.nfe.acbrlibnfe.demo.databinding.ActivityComandosNfeBinding;
import com.google.android.material.tabs.TabLayoutMediator;

import br.com.acbr.lib.nfe.ACBrLibNFe;

public class ComandosNFeActivity extends AppCompatActivity {

    private ActivityComandosNfeBinding binding;

    private ACBrLibNFe ACBrNFe;

    @SuppressLint("MissingInflatedId")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_comandos_nfe);

        binding = ActivityComandosNfeBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        configTabLayout();

        ACBrNFe = ACBrLibHelper.getInstance("");
    }

    private void configTabLayout(){
        ViewPagerAdapter adapter = new ViewPagerAdapter(this);
        binding.viewPager.setAdapter(adapter);

        adapter.addFragment(new ComandosEnvioNFeFragment(), "Envio");
        adapter.addFragment(new ComandosConsultaNFeFragment(), "Consultas");
        adapter.addFragment(new ComandosEventoNFeFragment(), "Eventos");
        adapter.addFragment(new ComandosInutilizacaoNFeFragment(), "Inutilização");
        adapter.addFragment(new ComandosDistribuicaoNFeFragment(), "Distribuição DFe");

        binding.viewPager.setOffscreenPageLimit(adapter.getItemCount());

        TabLayoutMediator mediator = new TabLayoutMediator(binding.tabs, binding.viewPager, (tab, position) -> {
            tab.setText(adapter.getTitle(position));
        });

        mediator.attach();
    }
}
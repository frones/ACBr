package com.acbr.consultacnpj.acbrlibconsultacnpj.demo;

import android.annotation.SuppressLint;
import android.os.Bundle;

import androidx.activity.EdgeToEdge;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;

import com.acbr.consultacnpj.acbrlibconsultacnpj.demo.databinding.ActivityComandosConsultaCnpjBinding;
import com.google.android.material.tabs.TabLayoutMediator;

import br.com.acbr.lib.consultacnpj.ACBrLibConsultaCNPJ;

public class ComandosConsultaCNPJActivity extends AppCompatActivity {

    private ActivityComandosConsultaCnpjBinding binding;

    private ACBrLibConsultaCNPJ ACBrConsultaCNPJ;

    @SuppressLint("MissingInflatedId")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_comandos_consulta_cnpj);

        binding = ActivityComandosConsultaCnpjBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        configTabLayout();

        ACBrConsultaCNPJ = ACBrLibHelper.getInstance("");
    }

    private void configTabLayout(){
        ViewPagerAdapter adapter = new ViewPagerAdapter(this);
        binding.viewPager.setAdapter(adapter);

        adapter.addFragment(new ComandosConsultaCNPJFragment(), "Consultar CNPJ");

        binding.viewPager.setOffscreenPageLimit(adapter.getItemCount());

        TabLayoutMediator mediator = new TabLayoutMediator(binding.tabs, binding.viewPager, (tab, position) -> {
            tab.setText(adapter.getTitle(position));
        });

        mediator.attach();
    }
}
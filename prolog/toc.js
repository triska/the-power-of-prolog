var toc = new Array(
    { link: "/prolog",
      title: "Overview"
    },
    { link: "/prolog/introduction",
      title: "Introduction"
    },
    { link: "/prolog/facets",
      title: "Facets of Prolog"
    },
    { link: "/prolog/concepts",
      title: "Basic Concepts"
    },
    { link: "/prolog/data",
      title: "Data Structures"
    },
    { link: "/prolog/reading",
      title: "Reading Prolog Programs"
    },
    { link: "/prolog/writing",
      title: "Writing Prolog Programs"
    },
    { link: "/prolog/termination",
      title: "Termination"
    },
    { link: "/prolog/nontermination",
      title: "Nontermination"
    },
    { link: "/prolog/clpfd",
      title: "Integer Arithmetic"
    },
    { link: "/prolog/metapredicates",
      title: "Higher-order Predicates"
    },
    { link: "/prolog/purity",
      title: "Logical Purity"
    },
    { link: "/prolog/testing",
      title: "Declarative Testing"
    },
    { link: "/prolog/debugging",
      title: "Declarative Debugging"
    },
    { link: "/prolog/dcg",
      title: "Definite Clause Grammars"
    },
    { link: "/prolog/sorting",
      title: "Sorting and Searching"
    },
    { link: "/prolog/global",
      title: "Global Variables"
    },
    { link: "/tist/",
      title: "Thinking in States"
    },
    { link: "/acomip/",
      title: "Meta-interpreters"
    },
    { link: "/prolog/macros",
      title: "Macros"
    },
    { link: "/prolog/optimization",
      title: "Combinatorial Optimization"
    },
    { link: "/prolog/expertsystems",
      title: "Expert Systems"
    },
    { link: "/prolog/web",
      title: "Web Applications"
    },
    { link: "/prolog/cryptography",
      title: "Cryptography"
    },
    { link: "/prolog/business",
      title: "Business Cases"
    },
    { link: "/prolog/theoremproving",
      title: "Theorem Proving"
    },
    { link: "/prolog/puzzles",
      title: "Logic Puzzles"
    },
    { link: "/prolog/efficiency",
      title: "Efficiency"
    },
    { link: "/prolog/memoization",
      title: "Memoization"
    },
    { link: "/prolog/ai",
      title: "Artificial Intelligence"
    },
    { link: "/prolog/horror",
      title: "Horror Stories"
    },
    { link: "/prolog/fun",
      title: "Fun Facts"
    },
    { link: "/prolog/engineering",
      title: "Engineering Aspects"
    },
    { link: "/prolog/future",
      title: "The Future"
    },
    { link: "/prolog/showcases/",
      title: "Showcases"
    },
    { link: "/prolog/attributedvariables",
      title: "Attributed Variables"
    }
);

$('body').prepend("<br><br> \
<center>\
<div id=\"navigation\"> \
  <div class=\"toc\"> \
  <button onclick=\"toc_dropdown()\" class=\"tocbutton\">The Power of Prolog</button> \
  <div id=\"toclist\" class=\"toc-content\"></div> \
  </div> \
</div> \
</center>");


var i = 0;
var current_index = -1;

while (i < toc.length) {
    if ( toc[i].link == $(location).attr('pathname') ) {
        current_index = i;
        break;
    }
    i++;
}


$.each(toc, function(i, v){
    var link = $('<a>', { text: v.title, href: v.link})

    if (v.title == "Showcases") {
        $('#toclist').append($('<hr>'));
    }

    $('#toclist').append( 
        i == current_index
            ? $('<div>', { class: "currentlink" }).append(link) : link )

    if (i == 0 ) {
        $('#toclist').append($('<hr>'))
    }
});



var next = current_index + 1;
var prev = current_index - 1;

( next < toc.length ) &&
    $('#navigation').append($('<a>', { text: ">",
                                       class: "navigation",
                                       href: toc[next].link}));

( prev >= 0 ) &&
    $('#navigation').prepend($('<a>', { text: "<",
                                        class: "navigation",
                                        href: toc[prev].link}));

function toc_dropdown() {
    $("#toclist").toggle();
}

// Close the TOC menu if the user clicks outside of it
$(window).bind("click touchstart", 
               function(event) {
                   if ( !event.target.matches('.tocbutton') &&
                        ($(event.target).parent().attr('id') != 'toclist') ) {
                       $("#toclist").hide();
                   }
               });

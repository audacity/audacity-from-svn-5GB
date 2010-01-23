
#include <stdlib.h>

typedef struct Node {
    int value;
    struct Node *next;
}Node;

typedef struct SRSW_Queue {
    Node *head, *tail;
}SRSW_Queue;


SRSW_Queue *srsw_queue_new()
{
    SRSW_Queue *q = (SRSW_Queue*)malloc(sizeof(SRSW_Queue));
    Node *dummy = (Node*)malloc(sizeof(Node));

    dummy->next = NULL;

    q->head = dummy;
    q->tail = dummy;

    return q;
}

void srsw_queue_enqueue(SRSW_Queue *q, int value)
{
    Node *n = (Node*)malloc(sizeof(Node));

    if(!n)
        printf("out of memory!!\n");
    n->value = value;
    n->next = NULL;

    /* let the store settle here */

    q->tail->next = n;
    q->tail = n;
}

int srsw_queue_dequeue(SRSW_Queue *q, int *value)
{
    Node *n = q->head;
    Node *new_head = n->next;

    if(new_head == NULL)
        return 0;

    *value = new_head->value;
    q->head = new_head;
    free(n);

    return 1;
}

int srsw_queue_get_count(SRSW_Queue *q)
{
    Node *n = q->head;
    int num = 0;

    /* the head node doesn't count; it is a dummy */
    while(n->next)
    {
        n = n->next;
        num++;
    }

    return num;
}

